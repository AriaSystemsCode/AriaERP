*:***************************************************************************
*: Program file  : ARPINVFG
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR KAZROUNI(F&G)
*! Date          : 12/25/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVFG
*:***************************************************************************
*: This Report Program is due to C200102 ...
*:***************************************************************************
*: Modification : 
*:B603535,1 BWA 04/02/2000 Fix this bugs in the invoice :
*:                         1- Printing all size buckets description (up to 11 sizes)
*:                            from scale file sorted by scale number even if the size
*:                            is not used in the invoice printed. 
*:                         2- Don't print Discount or Taxes lines if they have zero 
*:                            amount in the invoice printed.[Fix in ARPINVFG.FRX]
*:B603614,1 BWA 04/05/2000 Fix the bug of printing the quantity and the price in the rigth place.
*:B603642,1 BWA 04/16/2000 1)If you didn't have any discount we want not display the following line:-
*:                           the net after discount. [The fix in the FRX]
*:                         2)The total sizes quantity are count incorrect.
*:************************************************************************************

DIMENSION laScale [1,12]
DIMENSION laStlQty[1,12]
STORE SPACE(0) TO laScale , laStlQty

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

*B603535,1 Printing all size buckets description (up to 11 sizes).[START]
*PRIVATE lcAlias , lcCentury , lcORDER
PRIVATE lcAlias , lcCentury , lcORDER , lcAliasSc , lcAliasEq
*B603535,1 [END]

lcAlias = ALIAS()

CREATE CURSOR (lcLogBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'FGLOGO.BMP')

CREATE CURSOR (lclines) (Invoice C(6) , Account C(5)  , ALtStyle C(6) , Style C(6)    ,;
                         Scale C(3)   , SZ1 C(5)      , SZ2 C(5)      , SZ3 C(5)      ,;
                         SZ4 C(5)     , SZ5 C(5)      , SZ6 C(5)      , SZ7 C(5)      ,;
                         SZ8 C(5)     , SZ9 C(5)      , SZ10 C(5)     , SZ11 C(5)     ,;
                         Qty1 N(6)    , Qty2 N(6)     , Qty3 N(6)     , Qty4 N(6)     ,;
                         Qty5 N(6)    , Qty6 N(6)     , Qty7 N(6)     , Qty8 N(6)     ,;
                         Qty9 N(6)    , Qty10 N(6)    , Qty11 N(6)    , Totqty N(7)   ,;
                         Price1 N(9,2), Price2 N(9,2) , Price3 N(9,2) , Price4 N(9,2) ,;
                         Price5 N(9,2), Price6 N(9,2) , Price7 N(9,2) , Price8 N(9,2) ,;
                         Price9 N(9,2), Price10 N(9,2), Price11 N(9,2), Order C(6)    ,;
                         Store C(8)   , Desc C(20)    , LineNo N(6,0))

INDEX ON Invoice+ LEFT(Scale,1) + IIF(!EMPTY(ALTSTYLE),ALTSTYLE,Style) TAG Invoice of (gcWorkDir+lclines)

*Creat a new index to sort the invline to help in collecting the data[start]
  SELECT InvLine
  lcORDER = ORDER()
  IF !FILE(gcWorkDir +lcInvLfg+ '.CDX')
    INDEX ON INVOICE + STYLE + SCALE TAG cSTYLE OF (gcWorkDir +lcInvLfg + '.CDX')
  ENDIF
*Creat a new index to sort the invline to help in collecting the data[END]

SELECT InvHdr
SET SKIP TO
lcScanExp = IIF(UPPER(lcRpExp)==".T.", "", "FOR " + lcRpExp)

SCAN &lcScanExp
  *HOLD THE DATE IN A VARIABLE TO SHOW THE FULL YEAR.
  lcCentury  = SET('CENTURY')
  SET CENTURY ON
  ldDate    = DTOC(InvHdr.InvDate)
  SET CENTURY &lcCentury

  lcInvoice = INVHDR.invoice
  SELECT InvLine
  lcScale   = SPACE(1)
  lcStyle   = SPACE(1)

  =SEEK(lcInvoice)
  SCAN  WHILE invoice = lcInvoice

    llFRSTEXT = .F.
    llExit    = .F.                        && Flag to check if the M.SZ is empty or not
    llFlgRep  = .F.                        && Flag to check if the replace is ok.
    RELEASE ALL LIKE Q*                    && Release all the memory variables for qyantity
    RELEASE ALL LIKE S*                    && Release all the memory variables for scale and size

    SCATTER FIELDS LIKE Invoice   , Account   , ALtStyle  , Style  , Qty1  , Qty2   , Qty3      ,;
                        Qty4      , Qty5      , Qty6      , Qty7   , Qty8  , Totqty , Price     ,;
                        Order     , Store     , Scale     , LineNo     MEMVAR
    SELECT(lcLines)
    IF (InvLine.Scale # lcScale .AND. LEFT(InvLine.Style,6) # lcStyle) .OR. (LEFT(InvLine.Style,6) # lcStyle)
                     && in first record OR the style change

      IF SEEK(Invoice + SUBSTR(InvLine.Scale,1,1)) .AND. !EMPTY(lcScale) .AND.  (LEFT(InvLine.Style,6) = lcStyle)  && SEEK THE FIRST CHARACTER OF THE SCALE AND GET THE RECNO OF IT
                                                                                                                   && And check if the Lriables that hold the scale and the styule is empty or not to make a new recored.
        FOR I = 1 TO 11
          IF llFRSTEXT .OR. llExit
            EXIT
          ENDIF

          FOR N = 1 TO 11
            IF llExit .OR. llFRSTEXT
              EXIT
            ENDIF

            * lcSzT variable hold the size of the temp. file which we check on it the size from the scale file.
            * lcSzI variable hold the size of the scale file that we search with it in the temp. file to check if it is found or not.

            lcSzT  = 'SZ' + ALLTRIM(STR(N))        && variable for the lclines
            lcSzI  = 'SZ' + ALLTRIM(STR(I))        && variable for the scale

            lcQtyT = 'QTY' + ALLTRIM(STR(N))        && variable for the lclines
            lcQtyI = 'QTY' + ALLTRIM(STR(I))        && variable for the Invline
            lcPrc   = ALLTRIM(STR(N))

            *Check here if t is the variable hold the size is empty or not <that meanes there is no another size>.
            IF EMPTY(SCALE.&lcSzI)
              llFRSTEXT = .T.
              EXIT
            ELSE
              IF !EMPTY(&lcLines..&lcSzT)            && case the size not empty.
                IF SCALE.&lcSzI = &lcLines..&lcSzT
                  APPEND BLANK
                  RELEASE ALL LIKE Q*                    && Release all the memory variables for Qty. for not to update in two fields.
                  GATHER MEMVAR MEMO
                  REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI               ,;
                          &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                          &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                          Desc    WITH STYLE.DESC
                  I = I + 1
                ENDIF
              ELSE                         && case the recored has many sizes.
                lcRecNo1 = RECNO(lcLines)
                IF SEEK(Invoice + SUBSTR(InvLine.Scale,1,1))
                lcRecSeek = RECNO(lcLines)       && the recno after the seek

                  FOR lnC = I TO 11              && lnc meanes complete the fields.
                    IF llFRSTEXT .OR. llExit
                      EXIT
                    ENDIF

                    FOR lnN = N TO 11
                      IF llExit .OR. llFRSTEXT
                        EXIT
                      ENDIF

                      lcSzT  = 'SZ' + ALLTRIM(STR(lnN))        && variable for the lclines
                      lcSzI  = 'SZ' + ALLTRIM(STR(lnC))        && variable for the scale

                      lcQtyT = 'QTY' + ALLTRIM(STR(lnN))        && variable for the lclines
                      lcQtyI = 'QTY' + ALLTRIM(STR(lnC))        && variable for the Invline
                      lcPrc   = ALLTRIM(STR(lnN))

                      IF EMPTY(SCALE.&lcSzI)
                        llFRSTEXT = .T.
                        EXIT
                      ELSE
                      GOTO lcRecSeek
                        IF !EMPTY(&lcLines..&lcSzT)            && case the sz not empty.
                          IF SCALE.&lcSzI = &lcLines..&lcSzT
                            GOTO lcRecNo1
                              REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI               ,;
                                      &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                                      &lcLines..Price&lcPrc WITH InvLine.Price
                               lnC = lnC + 1
                          ENDIF
                        ELSE
                          llExit = .T.
                          EXIT
                        ENDIF
                      ENDIF
                    ENDFOR
                  ENDFOR
                ENDIF
              ENDIF
            ENDIF
          ENDFOR
        ENDFOR
      ELSE
      lcRecNew = 0
        IF !EOF(lcLines).AND. EVAL(lcLines+'.INVOICE') = lcInvoice   &&the 2 condition to check if the invoice change or not.
          SELECT SCALE
          =SEEK('S' + InvLine.Scale,'SCALE')
          SCATTER FIELDS LIKE SZ1 , SZ2 , SZ3 , SZ4 , SZ5 , SZ6 , SZ7 , SZ8 MEMVAR
          SELECT (lcLines)

          IF SEEK(Invoice + SUBSTR(InvLine.Scale,1,1))     && SEEK THE FIRST CHARACTER OF THE SCALE AND GET THE RECNO OF IT.
            lcRecSeek  = RECNO(lcLines)      && to hold the recno of the recored of the seek

            FOR I = 1 TO 11
              IF llFRSTEXT .OR. llExit
                EXIT
              ENDIF

              FOR N = 1 TO 11
                IF  I > 1
                  IF !EMPTY(SCALE.&lcSzI)
                    GOTO lcRecSeek
                  ENDIF
                ENDIF
                IF llExit
                  EXIT
                ENDIF

                lcSzT  = 'SZ' + ALLTRIM(STR(N))        && variable for the lclines
                lcSzI  = 'SZ' + ALLTRIM(STR(I))        && variable for the scale

                lcQtyT = 'QTY' + ALLTRIM(STR(N))        && variable for the lclines
                lcQtyI = 'QTY' + ALLTRIM(STR(I))        && variable for the Invline
                lcPrc   = ALLTRIM(STR(N))

                IF EMPTY(SCALE.&lcSzI)
                  IF lcRecNew # 0
                    GOTO lcRecNew
                  ENDIF
                  llFRSTEXT = .T.
                  EXIT
                ELSE
                  IF !EMPTY(&lcLines..&lcSzT)            && case the size not empty.
                    IF SCALE.&lcSzI = &lcLines..&lcSzT
                      IF  I > 1
                        GOTO lcRecNew
                      ENDIF

                        IF I = 1
                          APPEND BLANK

                          *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                          *REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI              ,;
                                  &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                                  &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                                  Totqty      WITH Totqty + InvLine.Totqty              ,;
                                  INVOICE               WITH INVLINE.INVOICE            ,;
                                  ACCOUNT               WITH INVLINE.ACCOUNT            ,;
                                  ALtStyle              WITH INVLINE.ALtStyle           ,;
                                  Style                 WITH LEFT(INVLINE.Style,6)      ,;
                                  ORDER                 WITH INVLINE.ORDER              ,;
                                  STORE                 WITH INVLINE.STORE              ,;
                                  SCALE                 WITH INVLINE.SCALE              ,;
                                  Desc                  WITH STYLE.DESC

                          REPLACE &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                                  &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                                  Totqty                WITH Totqty + InvLine.&lcQtyI   ,;
                                  INVOICE               WITH INVLINE.INVOICE            ,;
                                  ACCOUNT               WITH INVLINE.ACCOUNT            ,;
                                  ALtStyle              WITH INVLINE.ALtStyle           ,;
                                  Style                 WITH LEFT(INVLINE.Style,6)      ,;
                                  ORDER                 WITH INVLINE.ORDER              ,;
                                  STORE                 WITH INVLINE.STORE              ,;
                                  SCALE                 WITH INVLINE.SCALE              ,;
                                  Desc                  WITH STYLE.DESC
                          *B603535,1 [END]
                        ELSE
                          *B603642,1 2)The total sizes quantity are count incorrect.[START]
                          *REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI               ,;
                                  &lcLines..&lcQtyT     WITH InvLine.&lcQtyI            ,;
                                  &lcLines..Price&lcPrc WITH InvLine.Price

                          REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI               ,;
                                  &lcLines..&lcQtyT     WITH InvLine.&lcQtyI            ,;
                                  &lcLines..Totqty      WITH Totqty + InvLine.&lcQtyI   ,;
                                  &lcLines..Price&lcPrc WITH InvLine.Price
                          *B603642,1 [END]

                        ENDIF

                        lcRecNew  = RECNO(lcLines)      && to hold the recno of the new recored.
                        I = I + 1
                        N = 0
                    ENDIF
                  ELSE
                    REPLACE &lcLines..&lcSzT    WITH SCALE.&lcSzI

                    APPEND BLANK

                    *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                    *REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI              ,;
                            &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                            &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                            Totqty                WITH Totqty + InvLine.Totqty    ,;
                            INVOICE               WITH INVLINE.INVOICE            ,;
                            ACCOUNT               WITH INVLINE.ACCOUNT            ,;
                            ALtStyle              WITH INVLINE.ALtStyle           ,;
                            Style                 WITH LEFT(INVLINE.Style,6)      ,;
                            ORDER                 WITH INVLINE.ORDER              ,;
                            STORE                 WITH INVLINE.STORE              ,;
                            SCALE                 WITH INVLINE.SCALE              ,;
                            Desc                  WITH STYLE.DESC

                    REPLACE &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                            &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                            Totqty                WITH Totqty + InvLine.&lcQtyI   ,;
                            INVOICE               WITH INVLINE.INVOICE            ,;
                            ACCOUNT               WITH INVLINE.ACCOUNT            ,;
                            ALtStyle              WITH INVLINE.ALtStyle           ,;
                            Style                 WITH LEFT(INVLINE.Style,6)      ,;
                            ORDER                 WITH INVLINE.ORDER              ,;
                            STORE                 WITH INVLINE.STORE              ,;
                            SCALE                 WITH INVLINE.SCALE              ,;
                            Desc                  WITH STYLE.DESC
                     *B603535,1 [END]

                     lcRecNew  = RECNO(lcLines)      && to hold the recno of the new recored.
                     I = I + 1
                     N = 0
                  ENDIF
                ENDIF
              ENDFOR
            ENDFOR
          ENDIF
        ELSE
          APPEND BLANK
          *B603614,1 Print some fields wanted in the Temp. file not all the fields.[START]
          *GATHER MEMVAR MEMO
           GATHER MEMVAR FIELDS LIKE INVOICE,ACCOUNT,ALtStyle,STYLE,LINENO,Scale,Order, Store,TOTQTY
          *B603614,1 [END]

          *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
          *REPLACE SZ1     WITH SCALE.SZ1                                ,;
                  SZ2     WITH SCALE.SZ2                                 ,;
                  SZ3     WITH SCALE.SZ3                                 ,;
                  SZ4     WITH SCALE.SZ4                                 ,;
                  SZ5     WITH SCALE.SZ5                                 ,;
                  SZ6     WITH SCALE.SZ6                                 ,;
                  SZ7     WITH SCALE.SZ7                                 ,;
                  SZ8     WITH SCALE.SZ8                                 ,;
                  Desc    WITH STYLE.DESC                                ,;
                  Price1  WITH IIF(!EMPTY(SCALE.SZ1) ,InvLine.Price, 0 ) ,;
                  Price2  WITH IIF(!EMPTY(SCALE.SZ2) ,InvLine.Price, 0 ) ,;
                  Price3  WITH IIF(!EMPTY(SCALE.SZ3) ,InvLine.Price, 0 ) ,;
                  Price4  WITH IIF(!EMPTY(SCALE.SZ4) ,InvLine.Price, 0 ) ,;
                  Price5  WITH IIF(!EMPTY(SCALE.SZ5) ,InvLine.Price, 0 ) ,;
                  Price6  WITH IIF(!EMPTY(SCALE.SZ6) ,InvLine.Price, 0 ) ,;
                  Price7  WITH IIF(!EMPTY(SCALE.SZ7) ,InvLine.Price, 0 ) ,;
                  Price8  WITH IIF(!EMPTY(SCALE.SZ8) ,InvLine.Price, 0 )

          *B603614,1 Comment the replacing of the price fields to replace it with the qauntity fields.[START]
          *REPLACE Desc    WITH STYLE.DESC                               ,;
                  Price1  WITH IIF(!EMPTY(SCALE.SZ1) ,InvLine.Price, 0 ) ,;
                  Price2  WITH IIF(!EMPTY(SCALE.SZ2) ,InvLine.Price, 0 ) ,;
                  Price3  WITH IIF(!EMPTY(SCALE.SZ3) ,InvLine.Price, 0 ) ,;
                  Price4  WITH IIF(!EMPTY(SCALE.SZ4) ,InvLine.Price, 0 ) ,;
                  Price5  WITH IIF(!EMPTY(SCALE.SZ5) ,InvLine.Price, 0 ) ,;
                  Price6  WITH IIF(!EMPTY(SCALE.SZ6) ,InvLine.Price, 0 ) ,;
                  Price7  WITH IIF(!EMPTY(SCALE.SZ7) ,InvLine.Price, 0 ) ,;
                  Price8  WITH IIF(!EMPTY(SCALE.SZ8) ,InvLine.Price, 0 )

          REPLACE Desc    WITH STYLE.DESC

          *B603614,1 [END]
          *B603535,1 [END]

          *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
          lcAliasSc = ALIAS()
          SELECT SCALE
          =SEEK('S' + SUBSTR(InvLine.Scale,1,1) , 'SCALE')
          SCAN REST WHILE type+scale+prepak = 'S' + LEFT(InvLine.Scale,1)
              llScalExt = .F.
              FOR lnScale = 1 TO 11
                IF llScalExt
                  EXIT
                ENDIF

                lcSzI  = 'SZ' + ALLTRIM(STR(lnScale))        && variable for the scale.
                IF EMPTY(SCALE.&lcSzI)
                  llScalExt = .T.
                  EXIT
                ELSE
                  SELECT(lcLines)
                  FOR lnAll = 1 TO 11
                    lcSzT  = 'SZ' + ALLTRIM(STR(lnAll))
                    IF EMPTY(&lcLines..&lcSzT)
                      REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI
                      EXIT
                    ENDIF
                  ENDFOR
                ENDIF
              ENDFOR
          ENDSCAN
          SELECT (lcAliasSc)
          *B603535,1 [END]

          *B603614,1 Printing quantity and price value in the rigth place.[START]
          lcAliasSc = ALIAS()
          SELECT SCALE
          =SEEK('S' + InvLine.Scale,'SCALE')
          SCATTER FIELDS LIKE SZ1 , SZ2 , SZ3 , SZ4 , SZ5 , SZ6 , SZ7 , SZ8 MEMVAR
          SELECT (lcLines)

            FOR I = 1 TO 11
              IF llFRSTEXT
                EXIT
              ENDIF

              FOR N = 1 TO 11

                lcSzT  = 'SZ' + ALLTRIM(STR(N))        && variable for the lclines
                lcSzI  = 'SZ' + ALLTRIM(STR(I))        && variable for the scale

                lcQtyT = 'QTY' + ALLTRIM(STR(N))        && variable for the lclines
                lcQtyI = 'QTY' + ALLTRIM(STR(I))        && variable for the Invline
                lcPrc   = ALLTRIM(STR(N))

                IF EMPTY(SCALE.&lcSzI)
                  llFRSTEXT = .T.
                  EXIT
                ELSE
                  IF SCALE.&lcSzI = &lcLines..&lcSzT
                        REPLACE &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI ,;
                                &lcLines..Price&lcPrc WITH InvLine.Price
                      I = I + 1
                      N = 0
                  ENDIF
                ENDIF
              ENDFOR
            ENDFOR
          SELECT (lcAliasSc)
          *B603614,1 [END]

        ENDIF
      ENDIF
    ELSE       && if the style change and the scale did not change. we make update only.
      IF InvLine.Scale = lcScale

        *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
        lcAliasEq = ALIAS()
        SELECT SCALE
        =SEEK('S' + InvLine.Scale,'SCALE')
        SCATTER FIELDS LIKE SZ1 , SZ2 , SZ3 , SZ4 , SZ5 , SZ6 , SZ7 , SZ8 MEMVAR
        SELECT (lcLines)
        *B603535,1 [END]

        lcRecNo3 = RECNO(lcLines)
        IF SEEK(Invoice + SUBSTR(InvLine.Scale,1,1))
          lcRecSek = RECNO(lcLines)       && the recno after the seek

          FOR lnB = 1 TO 11
            IF llFRSTEXT .OR. llExit
              EXIT
            ENDIF

            FOR lnD = 1 TO 11
              IF llExit .OR. llFRSTEXT
                EXIT
              ENDIF

              lcSzT  = 'SZ' + ALLTRIM(STR(lnD))        && variable for the lclines
              lcSzI  = 'SZ' + ALLTRIM(STR(lnB))        && variable for the scale

              lcQtyT = 'QTY' + ALLTRIM(STR(lnD))        && variable for the lclines
              lcQtyI = 'QTY' + ALLTRIM(STR(lnB))        && variable for the Invline
              lcPrc   = ALLTRIM(STR(lnD))

              IF EMPTY(SCALE.&lcSzI)

                 *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                 *REPLACE Totqty      WITH Totqty + InvLine.Totqty
                 *B603535,1 [END]

                llFRSTEXT = .T.
                EXIT
              ELSE
                GOTO lcRecSek
                IF !EMPTY(&lcLines..&lcSzT)            && case the sz not empty.
                  IF SCALE.&lcSzI = &lcLines..&lcSzT
                    GOTO lcRecNo3
                    REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI               ,;
                            &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                            &lcLines..Price&lcPrc WITH InvLine.Price

                    *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                    REPLACE Totqty      WITH Totqty + InvLine.&lcQtyI
                    LND = 0
                    *B603535,1 [END]

                    lnB = lnB + 1
                  ENDIF
                ELSE
                  llExit = .T.
                  EXIT
                ENDIF
              ENDIF
            ENDFOR
          ENDFOR
        ENDIF

        *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
        SELECT(lcAliasEq)
        *B603535,1 [END]

      ELSE
        IF SUBSTR(InvLine.Scale,1,1) = SUBSTR(lcScale,1,1)
          SELECT SCALE
          =SEEK('S' + InvLine.Scale,'SCALE')
          SCATTER FIELDS LIKE SZ1 , SZ2 , SZ3 , SZ4 , SZ5 , SZ6 , SZ7 , SZ8 MEMVAR
          SELECT (lcLines)

          lcRecNo2 = RECNO(lcLines)
          IF SEEK(Invoice + SUBSTR(InvLine.Scale,1,1))     && SEEK THE FIRST CHARACTER OF THE SCALE AND GET THE RECNO OF IT.
            FOR I = 1 TO 11
              IF llFRSTEXT .OR. llExit
                EXIT
              ENDIF

              FOR N = 1 TO 11
                IF llExit
                  EXIT
                ENDIF

                lcSzT  = 'SZ' + ALLTRIM(STR(N))        && variable for the lclines
                lcSzI  = 'SZ' + ALLTRIM(STR(I))        && variable for the scale

                lcQtyT = 'QTY' + ALLTRIM(STR(N))        && variable for the lclines
                lcQtyI = 'QTY' + ALLTRIM(STR(I))        && variable for the Invline
                lcPrc   = ALLTRIM(STR(N))

                IF EMPTY(SCALE.&lcSzI)
                  llFRSTEXT = .T.
                  EXIT
                ELSE
                  IF !EMPTY(&lcLines..&lcSzT)            && case the size not empty.
                    IF SCALE.&lcSzI = &lcLines..&lcSzT
                      GOTO lcRecNo2
                      IF (SCALE.&lcSzI = &lcLines..&lcSzT)  .OR. EMPTY(&lcLines..&lcSzT)

                        *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                        *REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI              ,;
                                &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                                &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                                Totqty      WITH Totqty + InvLine.Totqty

                        REPLACE &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                                &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                                Totqty      WITH Totqty + InvLine.&lcQtyI
                       *B603535,1 [END]

                      ELSE
                        lcno = VAL(SUBSTR(lcSzT,3,1))
                        FOR lcRep  = lcNo TO 11
                          IF llFlgRep
                            EXIT
                          ENDIF
                          lcNoSz  = 'SZ' + ALLTRIM(STR(lcRep))
                          lcNoQty = 'QTY' + ALLTRIM(STR(lcRep))
                          lcPryc   = ALLTRIM(STR(lcRep))

                          IF EMPTY(&lcLines..&lcNoSz)
                            REPLACE &lcLines..&lcNoSz      WITH &lcLines..&lcSzT       ,;
                                    &lcLines..&lcNoQty     WITH &lcLines..&lcQtyT      ,;
                                    &lcLines..Price&lcPryc WITH &lcLines..Price&lcPrc

                            REPLACE &lcLines..&lcSzT       WITH  ""                    ,;
                                    &lcLines..&lcQtyT      WITH  0                     ,;
                                    &lcLines..Price&lcPrc  WITH  0
                            *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                            *REPLACE &lcLines..&lcSzT      WITH SCALE.&lcSzI              ,;
                                    &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                                    &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                                    Totqty      WITH Totqty + InvLine.Totqty

                            REPLACE &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI  ,;
                                    &lcLines..Price&lcPrc WITH InvLine.Price              ,;
                                    Totqty      WITH Totqty + InvLine.&lcQtyI
                            *B603535,1 [END]

                            llFlgRep = .T.
                          ENDIF
                        ENDFOR
                      ENDIF
                      I = I + 1

                      *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                      N = 0
                      *B603535,1 [END]

                    ENDIF
                  ELSE
                    FOR lnS = 1 TO 8
                      IF llExit
                        EXIT
                      ENDIF
                      FOR lnI = 1 TO 11
                        lcSclSz = 'SZ' + ALLTRIM(STR(lnI))
                        lcQty   = 'QTY'+ ALLTRIM(STR(lnI))
                        lcPrc   = ALLTRIM(STR(lnI))
                        IF EMPTY(&lcSclSz)                                     && check the size.
                          lcScl   = ALLTRIM(STR(lnS))
                          IF !EMPTY(M.SZ&lcScl)

                            *B603535,1 Printing all size buckets description (up to 11 sizes).[START]
                            *REPLACE &lcSclSz    WITH M.SZ&lcScl
                            GOTO lcRecNo2
                            *REPLACE &lcSclSz    WITH M.SZ&lcScl              ,;
                                    &lcQty      WITH &lcQty + M.QtY&lcScl     ,;
                                    Totqty      WITH Totqty + InvLine.Totqty  ,;
                                    Price&lcPrc WITH InvLine.Price

                            REPLACE &lcQty      WITH &lcQty + M.QtY&lcScl     ,;
                                    Totqty      WITH Totqty + InvLine.&lcScl  ,;
                                    Price&lcPrc WITH InvLine.Price
                            *B603535,1 [END]

                          ELSE
                            llExit = .T.
                            EXIT
                          ENDIF
                            lnS = lnS +1
                        ENDIF
                      ENDFOR
                    ENDFOR
                  ENDIF
                ENDIF
              ENDFOR
            ENDFOR
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    lcScale = InvLine.Scale
    lcStyle = LEFT(InvLine.Style,6)

  ENDSCAN
  SELECT INVHDR
ENDSCAN

*Erase the index I use it to collecting the data and reset to the origenal index [START]
IF FILE(gcWorkDir +lcInvLfg+ '.CDX')
  SELECT InvLine
  CLOSE INDEX
  ERASE (gcWorkDir +lcInvLfg+ '.CDX')
ENDIF

SELECT InvLine
SET ORDER TO &lcORDER
*Erase the index I use it to collecting the data and reset to the origenal index [END]

SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT InvHdr
SET RELATION TO INVHDR.Invoice INTO &lcLines ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + Invhdr.order INTO Ordhdr ADDITIVE

SELECT (lcLines)
SET RELATION TO IIF(!EMPTY(EVAL(lcLines+'.ALTSTYLE')) , EVAL(lcLines+'.ALTSTYLE') ,EVAL(lcLines+'.STYLE')) INTO STYLE ADDITIVE
SET RELATION TO "S" + EVAL(lcLines+'.Account') + EVAL(lcLines+'.STYLE') INTO SPCK_LIN ADDITIVE

SELECT InvHdr
SET SKIP TO (lcLines)

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrntUsr
*! Developer : Bassem Rafaat (BWA)
*! Date      : 12/28/1999
*! Purpose   : Function to get the descreption of the user ID
*!*************************************************************
*! Called from : ARPINVFG.FRX
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
*! Name      : lfArabic
*! Developer : Bassem Rafaat (BWA)
*! Date      : 12/15/1999
*! Purpose   : Function to get the descreption of the number in 
*!           : arbic words.  
*!*************************************************************
*! Called from : ARPINVFG.FRX
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
lcAlias = ALIAS()
SELECT InvHdr

No = STR(INVHDR.TotalChg,13,2)
STORE SPACE(0) TO Parts,PartStr,lcGetNO,lcAfter,Currency
Sex     = 1
lcTxt   = '' 
LNCOUNT = 0

IF VAL(NO) = 0              && Check if the value if [0] or not 
  lcGetNO = "’›—"
  RETURN lcGetNO
ENDIF

LCString = AllTrim(NO)
lcLength = Len(LCString)        && get the length of the number.
lcPos = ATC(".",LCString)       && assign the place of the dot in a variable

*«ﬁ”„ ”·”·… «·⁄œœ≈·Ï „«ﬁ»· «·›«’·… Ê„«»⁄œ «·›«’·…
IF lcPos > 0 
  lcAfter     = Right(LCString, lcLength - lcPos)
  LCString    = Left(LCString, lcPos - 1)
  lcLength    = Len(LCString)
  pos_After   = Len(lcAfter)
ENDIF

*Ã“¡ «·⁄œœ ≈·Ï ”·«”· Õ—›Ì… „ƒ·›… „‰ À·«À Œ«‰«  ⁄‘—Ì… √Ê √ﬁ·
TempLength = lcLength
Parts[1]   = LCString

Do While TempLength >= 3
  TempLength     = TempLength - 3
  LNCOUNT        = LNCOUNT + 1
  Parts[LNCOUNT] = Right(LCString, 3)
  LCString       = Left(LCString, TempLength)
ENDDO

Parts[LNCOUNT + 1] = LCString

*«” œ⁄ «· «»⁄ «·›—⁄Ì Ê«Õ›Ÿ «·‰ «∆Ã ›Ì «·„’›Ê›…
  For LNCOUNT = 1 To 4
    IF Len(Parts[LNCOUNT]) > 0
      PartStr[LNCOUNT] = lfGetNO(Parts[LNCOUNT], sex, LNCOUNT, @First,@First1,@Secnd,@Third)
    Else
      Exit For
    ENDIF
  ENDFOR

*Get the sub words in a one statment.
  For LNCOUNT = 5 To 1 Step -1
    IF Len(PartStr[LNCOUNT]) > 0
      IF LNCOUNT > 1 .AND. Len(PartStr[LNCOUNT - 1]) > 0
        lcTxt = lcTxt + " " + PartStr[LNCOUNT] + "Ê"
      Else 
        IF ATC(".",LCString) + ATC(",",LCString) = 0 
          IF Len(LCString) >= 3 
            IF Val(Right(LCString, 2)) > 10 
                Currency = "Ã‰ÌÂ« „’—Ì« "
            Else
              IF Val(Right(LCString, 2)) = 10 
                Currency = "Ã‰ÌÂ«  „’—Ì… "
              Else
                IF Val(Right(LCString, 2)) < 10 And Val(Right(LCString, 2)) > 2 
                  Currency = "Ã‰ÌÂ«  „’—Ì… "
                Else 
                  IF Val(Right(LCString, 2)) = 2 
                    Currency = "Ã‰ÌÂ« „’—Ì« "
                  Else
                    IF Val(Right(LCString, 2)) = 1 
                      Currency = "Ã‰ÌÂ „’—Ì "
                    Else
                      IF Len(LCString) = 2 And Val(LCString) > 10 
                        Currency = "Ã‰ÌÂ« „’—Ì« "
                      Else
                        IF Len(LCString) = 2 And Val(LCString) = 10 
                          Currency = "Ã‰ÌÂ«  „’—Ì… "
                        Else
                          IF Len(LCString) = 1 And Val(LCString) < 10 And Val(LCString) > 2 
                            Currency = "Ã‰ÌÂ«  „’—Ì… "
                          Else
                            IF Len(LCString) = 1 And Val(LCString) = 2 
                              Currency = "Ã‰ÌÂ« „’—Ì« "
                            Else
                              IF Len(LCString) = 1 And Val(LCString) = 1 
                                Currency = "Ã‰ÌÂ „’—Ì "
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
         lcTxt = "›ﬁÿ" + lcTxt + " " + PartStr[LNCOUNT] + " " + "Ã‰Ì…" + " " +Currency
         lcDub = OCCURS("Ã‰Ì",lcTxt)    &&for not dublicate the word "Ã‰Ì…"
         IF lcDub > 1 
           lcTxt =STRTRAN(lcTxt  , "Ã‰Ì…" , "" )
           IF !OCCURS("Ã‰Ì" ,lcTxt) = 1
               lcTxt = lcTxt +"Ã‰Ì…" + " " +Currency
           ENDIF  
         ENDIF
         
      ENDIF               
    ENDIF
  ENDFOR
  
  IF Val(lcAfter) > 0 
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
  Else
    InvSex = 1
  ENDIF
  
  *Solution for three numbers only.
  Lngth = Len(ns)

  *the {1}
  Indx[1] = Val(SUBSTR(ns,Lngth,1))
  IF Indx[1] = 0
    TmpArray[1] = ""
  ELSE
    TmpArray[1] = frst(Indx[1],sex)
  ENDIF
  
  Lngth = Lngth - 1
  IF Lngth > 0
    *the {10}
    Indx[2] = Val(SUBSTR(ns, Lngth, 1))
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
    Else
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
    
    IF Indx[2] > 1 And !EMPTY(TmpArray[1])    &&«·⁄‘—«  „‰ 1 ≈·Ï  ”⁄…
      TmpArray[1] = TmpArray[1] + " Ê"
    Else
      IF (Indx[1] = 1) And (Indx[2] = 1)        &&  '√Õœ ⁄‘—
        TmpArray[1] = frst1(1, sex)
      Else 
        IF (Indx[1] = 2) And (Indx[2] = 1)      &&' «À‰« ⁄‘—
          TmpArray[1] = frst1(2, sex)
        ENDIF
      ENDIF  
    ENDIF
    Lngth = Lngth - 1
    IF Lngth > 0
    *the {100}
      Indx[3] = Val(SUBSTR(ns, Lngth, 1))
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
      IF (Indx[3] > 0) And (!EMPTY(TmpArray[1]) Or !EMPTY(TmpArray[1])) 
        TmpArray[3] = TmpArray[3] + " Ê"
      ENDIF  
    Else
      lcGetNO = lfPower()
      RETURN lcGetNO
    ENDIF
  Else    
    lcGetNO = lfPower()
    RETURN lcGetNO
  ENDIF
  *≈÷«›… ﬂ·„… «·„— »…(„∆…,√·›,...)Õ”» „— »… «·√—ﬁ«„
    lcGetNO = lfPower()
    RETURN lcGetNO
  
**********************************************************
FUNCTION lfPower

DO CASE 
  Case Power = 0
    IF !EMPTY(TmpArray[1]) And Indx[1] = 1 And EMPTY(TmpArray[2]) And pos_After = 1 
      lcNO = scnd(Indx[1], sex) + "ﬁ—Ê‘ "
    Else
      IF !EMPTY(TmpArray[1]) And Indx[1] > 1 And EMPTY(TmpArray[2]) And pos_After = 1 
        lcNO = scnd(Indx[1], sex) + "ﬁ—‘« "        
      Else
        IF EMPTY(TmpArray[1]) And !EMPTY(TmpArray[2]) And Indx[1] = 0 And Indx[2] = 1 And pos_After = 2
          lcNO = scnd(Indx[2], sex) + "ﬁ—Ê‘ "          
        Else
          IF EMPTY(TmpArray[1]) And !EMPTY(TmpArray[2]) And Indx[2] = 0 And Indx[2] > 1 And pos_After = 2
            lcNO = scnd(Indx[2], sex) + "ﬁ—‘« "
          Else
            IF EMPTY(TmpArray[1]) And !EMPTY(TmpArray[2]) And Indx[1] = 0 And Indx[2] > 1 And pos_After = 2
              lcNO = scnd(Indx[2], sex) + "ﬁ—‘« "
            Else
              IF !EMPTY(TmpArray[1]) And EMPTY(TmpArray[2]) And Indx[1] = 1 And Indx[2] = 0 And pos_After = 2
                lcNO = frst(Indx[1], sex) + "ﬁ—‘ "
              Else
                IF !EMPTY(TmpArray[1]) And EMPTY(TmpArray[2]) And Indx[1] = 2 And Indx[2] = 0 And pos_After = 2
                  lcNO = frst(Indx[1], sex) + "ﬁ—‘« "
                Else
                  IF !EMPTY(TmpArray[1]) And EMPTY(TmpArray[2]) And Indx[1] > 2 And Indx[1] <= 9 And Indx[2] = 0 And pos_After = 2
                    lcNO = frst(Indx[1], sex) + "ﬁ—Ê‘ "
                  Else
                    IF !EMPTY(TmpArray[1]) And !EMPTY(TmpArray[2]) And Indx[1] = 1 And Indx[2] = 1 And pos_After = 2 ;
                      Or !EMPTY(TmpArray[1]) And !EMPTY(TmpArray[2]) And Indx[1] = 2 And Indx[2] = 1 And pos_After = 2
                      lcNO = frst1(Indx[1], sex) + scnd(Indx[2], InvSex) + "ﬁ—‘« "
                    Else
                      IF !EMPTY(TmpArray[1]) And !EMPTY(TmpArray[2]) And Indx[1] > 0 And Indx[2] >= 2 And pos_After = 2 
                        lcNO = frst(Indx[1], sex) + "Ê " + scnd(Indx[2], InvSex) + "ﬁ—‘« "
                      Else
                        IF !EMPTY(TmpArray[1]) And !EMPTY(TmpArray[2]) And Indx[1] > 0 And Indx[2] = 1 And pos_After = 2
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
  
  Case Power = 1
    lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2]
    IF !EMPTY(TmpArray[3]) AND !EMPTY(TmpArray[1]) AND EMPTY(TmpArray[2]) AND Indx[1] <= 2
      lcNO =    TmpArray[3] + TmpArray[1] + "Ã‰Ì…"
    ELSE
      IF !EMPTY(TmpArray[3]) AND !EMPTY(TmpArray[1]) AND EMPTY(TmpArray[2]) AND Indx[1] > 2
        lcNO =    TmpArray[3] + TmpArray[1] + "Ã‰ÌÂ« "
      ENDIF
    ENDIF
    IF Len(Parts[2]) = 0 .AND. EMPTY(TmpArray[2]) .AND. EMPTY(TmpArray[3])
      lcNO =STRTRAN(lcNO  , "Ê«Õœ " , "Ã‰Ì… Ê«Õœ ")
      lcNO =STRTRAN(lcNO  , "«À‰«‰ ", "Ã‰ÌÂ«‰")
    ENDIF
    IF Indx[1] > 2 .AND. EMPTY(TmpArray[2]) .AND. EMPTY(TmpArray[3])
      lcNO = lcNO + "Ã‰ÌÂ« "
    ENDIF
    
  Case Power = 2
    IF (Indx[1] = 1) And (Indx[2] = 0) And (Indx[3] = 0)
      lcNO = " √·› "        
    Else
      IF (Indx[1] = 2) And (Indx[2] = 0) And (Indx[3] = 0) 
        lcNO = " √·›«‰ "
      Else
        IF (Indx[1] > 2) And (Indx[2] = 0) And (Indx[3] = 0) 
          lcNO = TmpArray[1] + " ¬·«› "
        Else
          IF (Indx[1] = 0) And (Indx[2] = 1) And (Indx[3] = 0) 
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " ¬·«› "
          Else
            IF (Indx[1] = 0) And (Indx[2] = 0) And (Indx[3] = 0) 
              lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2]
            Else
              lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " √·› "
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    
  Case Power = 3
    IF (Indx[1] = 1) And (Indx[2] = 0) And (Indx[3] = 0) 
      lcNO = " „·ÌÊ‰ "
    Else
      IF (Indx[1] = 2) And (Indx[2] = 0) And (Indx[3] = 0) 
        lcNO = " „·ÌÊ‰«‰ "
      Else
        IF (Indx[1] > 2) And (Indx[2] = 0) And (Indx[3] = 0) 
          lcNO = TmpArray[1] + " „·«ÌÌ‰ "
        Else
          IF (Indx[1] = 0) And (Indx[2] = 1) And (Indx[3] = 0) 
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·«ÌÌ‰ "
          Else
            IF (Indx[1] = 0) And (Indx[2] = 0) And (Indx[3] = 0) 
              lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2]
            Else
              lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·ÌÊ‰ "
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF        

  Case Power = 4
    IF (Indx[1] = 1) And (Indx[2] = 0) And (Indx[3] = 0) 
      lcNO = " „·Ì«— "
    Else
      IF (Indx[1] = 2) And (Indx[2] = 0) And (Indx[3] = 0) 
        lcNO = " „·Ì«—«‰ "
      Else
        IF (Indx[1] > 2) And (Indx[2] = 0) And (Indx[3] = 0) 
          lcNO = TmpArray[1] + " „·Ì«—«  "
        Else
          IF (Indx[1] = 0) And (Indx[2] = 1) And (Indx[3] = 0) 
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·Ì«—«  "
          Else
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·Ì«— "
          ENDIF
        ENDIF
      ENDIF
    ENDIF
          
 EndCase
RETURN lcNO
