*!**************************************************************************
*: Name      : clumain.PRG
*: Developer : NeVIEN    NFZ
*: Date      : 05/17/2004
*: Purpose   :
*: ENTRY #   : C122115,1
*:**************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:**************************************************************************
*! C125761,1 AAH 02/07/2005 Receive cartons of WA*******  or (000) standared cartons id 
*! C125761,2 EIH 08/31/2005 fix the problem of in case of recieved Qty more than ordered QTY
*! C125761,2 				in text file
*:**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!*************************************************************
*! Name      : LFADDOPTN
*! Developer : Neveen (NFZ)
*! Date      : 11/05/2004
*! Purpose   : PICKING LIST FROM TXT FILE
*!*************************************************************
*! Calls     :
*!             Procedures : LPvld_cod
*!             Functions  : lfvApply1()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = DO lPpick_txt
*!*************************************************************
FUNCTION lfAddOptn

  DEFINE BAR lnBarNo+2 OF _OPTPOP PROMPT 'Update from \<text file'  ;
    SKIP FOR (laScrMode[1] .OR. laScrMode[2])
  ON SELECTION BAR lnBarNo+2 OF _OPTPOP DO gfDoTriger WITH  'ALPLIST',PADR('PICK_TXT',10)

  *!*************************************************************
  *! Name      : lFpick_txt
  *! Developer : Neveen (NFZ)
  *! Date      : 11/05/2004
  *! Purpose   : PICKING LIST FROM TXT FILE
  *!*************************************************************
  *! Calls     :
  *!             Procedures : LPvld_cod
  *!             Functions  : lfvApply1()
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = DO lPpick_txt
  *!*************************************************************
FUNCTION lfPick_Txt

  lcFilHandl=0
  STORE .F. TO M.LCCRT_FOUND,llVldFil,llrptstyl
  lnrcno=0
  m.LCTXTFIL=GETFILE('TXT', 'ENTER THE TXT FILE ', 'Open',0)
  IF EMPTY(M.LCTXTFIL)
    RETURN
  ENDIF
  lcTmp_Txt  = gfTempName()
  lcPick_Txt = gfTempName()
  CREATE TABLE (gcWorkDir + lcTmp_Txt) ;
    (UPC_code C(20))

  APPEND FROM &LCTXTFIL TYPE SDF
  USE

  USE (gcWorkDir+lcTmp_Txt)  IN 0 ALIAS lc_txt
  =gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'STYUPCN','SH')

  lcFilHandl = FCREAT('&gcWorkDir.'+lcPick_Txt+'.txt')
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,"*                  PACKING LIST MANUAL REPORT                  *")
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,' ')
  =FPUTS(lcFilHandl,' ')
  STOR 0 TO lnQty,lncrtqty,lnTo
  STOR 1 TO lnFrom,lnLnTo1
  m.lcUpc=lc_txt.UPC_code
  LC_CRTID=m.lcUpc  &&& CARTON _ID
  SELECT lc_txt  && Temp File
  SCAN
    IF LEN(ALLTRIM(lc_txt.UPC_code))=12 AND LEN(lc_txt.UPC_code )>0
      llVldFil=.T.
      EXIT
    ELSE
      llVldFil=.F.
    ENDIF
  ENDSCAN
  GO TOP
  IF !llVldFil
    SELECT lc_txt
    USE
    =FPUTS(lcFilHandl,'  INVALID TXT FILE .... TRY AGAIN ' )
    =lfViewRLog()
    RETURN
  ENDIF
  DO WHILE !EOF()  && lOOP FOR TEMP FILE

    IF m.lcUpc=lc_txt.UPC_code
      IF VAL(m.lcUpc)>0                      &&&  >0 FOR STYLE CODE    =0 FOR CArTON CODE
        m.lnQty=m.lnQty+1
      ENDIF
    ENDIF
    SELECT lc_txt
    IF !EOF()
      SKIP
    ELSE
      EXIT
    ENDIF     && FOR EOF CONDITION
    IF m.lcUpc<>lc_txt.UPC_code
    
      *C125761,1 AAH Receive cartons of WA*******  or (000) standared cartons id,02/07/2005 [begin]
      *IF VAL(m.lcUpc)>0
      IF VAL(m.lcUpc)>0 AND LEN(ALLTRIM(m.lcUpc))<> 20
        *C125761,1 AAH Receive cartons of WA*******  or (000) standared cartons id,02/07/2005 [end  ]

        DO lfVld_Cod                   && VALID PROGRAM  & APPLY FUNCTION]
        *lncrtqty=lncrtqty+lnQty            && total qty per carton
        IF VAL(lc_txt.UPC_code )>0
          llrptstyl=.F.
          SELECT lc_txt
        ENDIF
        lnQty=0
      ELSE
        lncrtqty=0
        LC_CRTID=m.lcUpc  && CARTON ID
        IF M.LCCRT_FOUND=.T.
          lnLnTo1=lnLnTo1+1                  && NEW CARTON
          STORE lnLnTo1 TO lnFrom,m.lnTo
          m.LCCRT_FOUND=.F.
        ENDIF
        SELECT lc_txt
      ENDIF
      m.lcUpc=lc_txt.UPC_code                 && NEW iTEM
    ENDIF
    SELECT lc_txt
  ENDDO
  =lfViewRLog()
  SELECT lc_txt
  USE
  RETURN

  *!*************************************************************
  *! Name      : lfVld_Cod
  *! Developer : Neveen (NFZ)
  *! Date      : 11/05/2004
  *! Purpose   : VAILDATE STYLE CODE & QTY THE SAME CODE IN ORDLIN
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : lfvApply1()
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = DO lPvld_cod
  *!*************************************************************
FUNCTION lfVld_Cod
  SELECT STYLEUPC     && styleupc File
  IF SEEK(SUBSTR(m.lcUpc,1,6)+SUBSTR(m.lcUpc,7,5)+SUBSTR(m.lcUpc,12,2))
    SELECT ORDLINE    && ordlin File
    IF SEEK('O'+ladata[2]+ladata[5]+STYLEUPC.STYLE)
      SCAN WHILE 'O' + ladata[2] + ladata[5] + STYLEUPC.STYLE = ;
          ORDLINE.cordtype + ORDLINE.ORDER + ORDLINE.STORE + ORDLINE.STYLE
        IF ladata[3]=ORDLINE.PIKTKT
          m.lcQty_fil='ORDLINE.qty'+ALLTRIM(STYLEUPC.SIZE)
          SELECT (lcPckLin)
          IF SEEK(STYLEUPC.STYLE)
            lnrecno=RECNO()  && recno for style
            lcSelFld    = 'cSelect'+ALLTRIM(STYLEUPC.SIZE)
            lcCtnQtyFld = 'CtnQty'+ALLTRIM(STYLEUPC.SIZE)
            lcOQtyFld   = 'OQty'+ALLTRIM(STYLEUPC.SIZE)
            lcSizeFld   = lcPckLin + '.cSize'   + ALLTRIM(STYLEUPC.SIZE)
            IF &lcOQtyFld < m.lnQty
              lcSize = IIF(SEEK('S'+STYLEUPC.SIZE,'SCALE'),('SCALE.SZ'+ALLTRIM(STYLEUPC.SIZE)),' ')
              
              *! C125761,2 EIH 08/31/2005 fix the problem of in case of recieved Qty more than ordered QTY
              *! C125761,2 				in text file [Start]
              *IF llrptstyl=.F.
              *  llrptstyl=.T.
              *  =FPUTS(lcFilHandl,ALLTRIM(STYLEUPC.CUPCNUM1)+ALLTRIM(STYLEUPC.CUPCNUM2)+;
              *    ALLTRIM(STYLEUPC.CUPCNUM3)+'  '+ALLTRIM(&lcPckLin..STYLE )+ '/' + EVAL(lcSizeFld)+ "    ORDER QTY. LESS THAN PICKING QTY."  )
              *ENDIF
              
              SELECT (lcPckLin)
              IF (OQTY1+OQTY2+OQTY3+OQTY4+OQTY5+OQTY6+OQTY7+OQTY8) > (PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8)
                REPLACE &lcSelFld    WITH '»'         ,;
                  &lcCtnQtyFld WITH IIF(&lcSelFld='»', IIF((lnLnTo1-lnFrom+1)>0,;
                  INT(m.lnQty/(lnLnTo1-lnFrom+1)),0),0),;
                  StyWgh       WITH IIF(&lcSelFld='',OrgStyWgh,StyWgh)
                REPLACE SELECTED WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                  cSelect3='»' OR cSelect4='»' OR ;
                  cSelect5='»' OR cSelect6='»' OR ;
                  cSelect7='»' OR cSelect8='»',   ;
                  1,0)
                =lfvApply1()
              ENDIF
              *! C125761,2 EIH 08/31/2005 [End]
              
            ELSE
              
              *! C125761,2 EIH 08/31/2005 fix the problem of in case of recieved Qty more than ordered QTY
              *! C125761,2 				in text file [Start]
              IF (OQTY1+OQTY2+OQTY3+OQTY4+OQTY5+OQTY6+OQTY7+OQTY8) > (PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8)           
                lncrtqty=lncrtqty+lnQty            && total qty per carton
                m.LCCRT_FOUND=.T.
                SELECT (lcPckLin)
                REPLACE &lcSelFld    WITH '»'         ,;
                  &lcCtnQtyFld WITH IIF(&lcSelFld='»', IIF((lnLnTo1-lnFrom+1)>0,;
                  INT(m.lnQty/(lnLnTo1-lnFrom+1)),0),0),;
                  StyWgh       WITH IIF(&lcSelFld='',OrgStyWgh,StyWgh)
                REPLACE SELECTED WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                  cSelect3='»' OR cSelect4='»' OR ;
                  cSelect5='»' OR cSelect6='»' OR ;
                  cSelect7='»' OR cSelect8='»',   ;
                  1,0)
                =lfvApply1()
              ENDIF
              *! C125761,2 EIH 08/31/2005 [End]  
            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
      lnQty=0
      lncrtqty=0
      =FPUTS(lcFilHandl,ALLTRIM(STYLEUPC.CUPCNUM1)+ALLTRIM(STYLEUPC.CUPCNUM2)+;
        ALLTRIM(STYLEUPC.CUPCNUM3)+'  '+ALLTRIM(STYLEUPC.STYLE )+' DOSE NOT EXIST IN ORDER FILE ... TRY AGAIN '  )
    ENDIF
  ELSE
    lnQty=0
    lncrtqty=0
    =FPUTS(lcFilHandl,ALLTRIM(STYLEUPC.CUPCNUM1)+ALLTRIM(STYLEUPC.CUPCNUM2)+;
      ALLTRIM(STYLEUPC.CUPCNUM3)+' DOSE NOT EXIST IN "UPC " FILE ... TRY AGAIN '  )
  ENDIF                 && styleupc File CONDITION
  RETURN

  *!*************************************************************
  *! Name      : lfvApply1
  *! Developer : neveen (nfz)
  *! Date      : 08/28/97
  *! Purpose   : validate pbApply data from txt file  per item
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : lfwDtlBrow
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = lfvApply1()
  *!*************************************************************

FUNCTION lfvApply1

  PRIVATE lnCurAlias,lnCurRec,lnJ,laCart,lnContinue,lcTag
  DIMENSION laCart[1]
  STORE 0 TO laCart,lnContinue
  lnCurAlias = SELECT(0)
  SELECT (lcPckLin)
  SET RELATION TO
  lnCurRec = RECNO()
  lnSizeRec = VAL(&lcScaFile..SzCode)
  IF lnFrom <= 0 OR lnLnTo1 <= 0 OR lnFrom > lnLnTo1
    *-- You have to enter correct carton range.
    *-- <OK>
    = gfModalGen("INM44033B00000","Dialog")
    _CUROBJ = OBJNUM(lnFrom)
  ELSE
    IF llPalStat
      SELECT COUNT(Cart_No) FROM (lcCtnHdr) INTO ARRAY laCart;
        WHERE Pal_No <> lnCtnPal;
        AND BETWEEN (Cart_No,lnFrom,lnLnTo1)

    ENDIF
    IF laCart[1] > 0
      *-- some cartons in carton range related to another palette.
      *-- OK
      = gfModalGen("INM44047B00000","Dialog")
    ELSE
      = lfUpdate() IN (gcprgDir+'alplist')
      *-- This part is to avoid entering a new palette number less
      *-- than the first palette number.
      *-- It also will avoid having gaps between palette numbers.
      *-- In order to check if there is a gap between two palette numbers
      *-- we subtract the last pallet number (lnMaxPal) from the currently
      *-- entered pallet number, the difference should not be greater than 1.
      *-- this rule is valid, except for the first pallet number to be
      *-- entered, because this rule will prevent the user from start
      *-- counting the pallets using any number greater than 1, that's why
      *-- we should not use that rule when this is the first pallet number
      *-- to be entered (in other words when lnMaxPal = 0).

      IF llPalStat AND lnCtnPal < lnMinPal OR (lnCtnPal - IIF(lnMaxPal=0,lnCtnPal,lnMaxPal)) > 1
        *-- palette No. has to be sequential.
        *-- OK
        = gfModalGen("INM44048B00000","Dialog")
        lnCtnPal = lnMaxPal + 1
        SHOW GET lnCtnPal
      ELSE
        lcTag = ORDER(lcPckLin)
        lnI  = 0
        FOR lnI = 1 TO &lcPckLin..SzCnt
          lcSelFld    = lcPckLin + '.cSelect' + STR(lnI,1)
          lcCtnQtyFld = lcPckLin + '.CtnQty'  + STR(lnI,1)
          lcOrdQtyFld = lcPckLin + '.OrdQty'  + STR(lnI,1)
          lcPQtyFld   = lcPckLin + '.PQty'    + STR(lnI,1)
          lcCtnQtyFld = lcPckLin + '.CtnQty'  + STR(lnI,1)
          lcSizeFld   = lcPckLin + '.cSize'   + STR(lnI,1)
          IF !EMPTY(&lcSelFld) AND !EMPTY(&lcCtnQtyFld) AND ;
              &lcOrdQtyFld < &lcPQtyFld + (&lcCtnQtyFld*(lnLnTo1-lnFrom+1))
            *-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
            Do you want to modify the orderd quantity?
            *-- <Yes>,<No>
            lnContinue = gfModalGen("QRM44034B00006","Dialog",&lcPckLin..STYLE + '/' + EVAL(lcSizeFld))
            EXIT
          ENDIF
        ENDFOR
        IF lnContinue <> 2
          SELECT(lcPckLin)
          *-- This for updating order qty with the new order qty if the user
          *-- select to update order qty with exceeds qty.
          SELECT (lcPckLin)
          IF lnContinue = 1
            SELECT (lcPckLin)
            REPLACE OrdQty1 WITH IIF(EMPTY(cSelect1) OR (!EMPTY(cSelect1) AND EMPTY(CtnQty1)),OrdQty1,MAX(OrdQty1,OrgPQty1+(CtnQty1*(lnLnTo1-lnFrom+1)))),;
              OrdQty2 WITH IIF(EMPTY(cSelect2) OR (!EMPTY(cSelect2) AND EMPTY(CtnQty2)),OrdQty2,MAX(OrdQty2,OrgPQty2+(CtnQty2*(lnLnTo1-lnFrom+1)))),;
              OrdQty3 WITH IIF(EMPTY(cSelect3) OR (!EMPTY(cSelect3) AND EMPTY(CtnQty3)),OrdQty3,MAX(OrdQty3,OrgPQty3+(CtnQty3*(lnLnTo1-lnFrom+1)))),;
              OrdQty4 WITH IIF(EMPTY(cSelect4) OR (!EMPTY(cSelect4) AND EMPTY(CtnQty4)),OrdQty4,MAX(OrdQty4,OrgPQty4+(CtnQty4*(lnLnTo1-lnFrom+1)))),;
              OrdQty5 WITH IIF(EMPTY(cSelect5) OR (!EMPTY(cSelect5) AND EMPTY(CtnQty5)),OrdQty5,MAX(OrdQty5,OrgPQty5+(CtnQty5*(lnLnTo1-lnFrom+1)))),;
              OrdQty6 WITH IIF(EMPTY(cSelect6) OR (!EMPTY(cSelect6) AND EMPTY(CtnQty6)),OrdQty6,MAX(OrdQty6,OrgPQty6+(CtnQty6*(lnLnTo1-lnFrom+1)))),;
              OrdQty7 WITH IIF(EMPTY(cSelect7) OR (!EMPTY(cSelect7) AND EMPTY(CtnQty7)),OrdQty7,MAX(OrdQty7,OrgPQty7+(CtnQty7*(lnLnTo1-lnFrom+1)))),;
              OrdQty8 WITH IIF(EMPTY(cSelect8) OR (!EMPTY(cSelect8) AND EMPTY(CtnQty8)),OrdQty8,MAX(OrdQty8,OrgPQty8+(CtnQty8*(lnLnTo1-lnFrom+1))))
          ENDIF
          lcOQtyFld   = 'OQty'+ALLTRIM(STYLEUPC.SIZE)
          lcPQtyFld   = 'pqty'+ALLTRIM(STYLEUPC.SIZE)

          REPLACE &lcPQtyFld WITH &lcPQtyFld+m.lnQty ,;
            &lcOQtyFld WITH IIF(&lcOQtyFld>m.lnQty,&lcOQtyFld-m.lnQty,0)

          REPLACE PWgh1 WITH IIF(EMPTY(cSelect1) AND EMPTY(CtnQty1),&lcPckLin..PWgh1,PWgh1 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty1*(lnLnTo1-lnFrom+1))),;
            PWgh2 WITH IIF(EMPTY(cSelect2) AND EMPTY(CtnQty2),&lcPckLin..PWgh2,PWgh2 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty2*(lnLnTo1-lnFrom+1))),;
            PWgh3 WITH IIF(EMPTY(cSelect3) AND EMPTY(CtnQty3),&lcPckLin..PWgh3,PWgh3 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty3*(lnLnTo1-lnFrom+1))),;
            PWgh4 WITH IIF(EMPTY(cSelect4) AND EMPTY(CtnQty4),&lcPckLin..PWgh4,PWgh4 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty4*(lnLnTo1-lnFrom+1)))
          REPLACE PWgh5 WITH IIF(EMPTY(cSelect5) AND EMPTY(CtnQty5),&lcPckLin..PWgh5,PWgh5 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty5*(lnLnTo1-lnFrom+1))),;
            PWgh6 WITH IIF(EMPTY(cSelect6) AND EMPTY(CtnQty6),&lcPckLin..PWgh6,PWgh6 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty6*(lnLnTo1-lnFrom+1))),;
            PWgh7 WITH IIF(EMPTY(cSelect7) AND EMPTY(CtnQty7),&lcPckLin..PWgh7,PWgh7 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty7*(lnLnTo1-lnFrom+1))),;
            PWgh8 WITH IIF(EMPTY(cSelect8) AND EMPTY(CtnQty8),&lcPckLin..PWgh8,PWgh8 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty8*(lnLnTo1-lnFrom+1)))
          STORE 0 TO lnQuantities,lnWeights
          lnQuantities = &lcPckLin..CtnQty1+&lcPckLin..CtnQty2+;
            &lcPckLin..CtnQty3+&lcPckLin..CtnQty4+;
            &lcPckLin..CtnQty5+&lcPckLin..CtnQty6+;
            &lcPckLin..CtnQty7+&lcPckLin..CtnQty8
          lnWeights    = &lcPckLin..StyWgh*&lcPckLin..CtnQty1+;
            &lcPckLin..StyWgh*&lcPckLin..CtnQty2+;
            &lcPckLin..StyWgh*&lcPckLin..CtnQty3+;
            &lcPckLin..StyWgh*&lcPckLin..CtnQty4+;
            &lcPckLin..StyWgh*&lcPckLin..CtnQty5+;
            &lcPckLin..StyWgh*&lcPckLin..CtnQty6+;
            &lcPckLin..StyWgh*&lcPckLin..CtnQty7+;
            &lcPckLin..StyWgh*&lcPckLin..CtnQty8
          IF lnQuantities > 0
            IF SEEK(STR(m.lnLnTo1,4),lcCtnHdr)
              SELECT (lcCtnHdr)
              REPLACE TotPcs WITH TotPcs + m.lnQty,;
                TotWgh WITH TotWgh + lnWeights   && add total qty per item to totpcs

              lnPackQty = lnPackQty + lnQuantities
              lnPackWgh = lnPackWgh + lnWeights

              SELECT(lcPckLin)
            ELSE
              *** change (lnFrom+lnJ) = m.lnLnTo1 # of carton    --- (lnQuantities) m.lncrtqty  Total Carton
              *            INSERT INTO (lcCtnHdr)(Cart_No   ,Pal_No  ,TotPcs      ,TotWgh   ,Empty);
              VALUES (m.lnLnTo1,lnCtnPal,m.lncrtqty,lnWeights,'N'  )

              INSERT INTO (lcCtnHdr)(Cart_No   ,Pal_No  ,TotPcs      ,TotWgh   ,EMPTY,CCARRCTNID);
                VALUES (m.lnLnTo1,lnCtnPal,m.lncrtqty,lnWeights,'N',LC_CRTID  )
              lnMaxCtn  = MAX(lnMaxCtn,m.lnLnTo1)
              lnPackWgh = lnPackWgh + &lcCtnHdr..TotWgh
              lnPackCtn = lnPackCtn + 1
              lnPackQty = lnPackQty + &lcCtnHdr..TotPcs
            ENDIF
            lnMaxPal  = MAX(lnMaxPal,&lcCtnHdr..Pal_No)
            lnMinPal  = MIN(IIF(lnMinPal=0,&lcCtnHdr..Pal_No,lnMinPal),&lcCtnHdr..Pal_No)
          ENDIF
          SELECT (lcCtnDtl)
          IF !SEEK(STR(m.lnLnTo1,4)+&lcPckLin..STYLE+STR(&lcPckLin..nOrdLineNo,6),lcCtnDtl)
            APPEND BLANK
          ENDIF
          SELECT  (lcPckLin)
          GO lnrecno
          SELECT (lcCtnDtl)
          REPLACE STYLE      WITH &lcPckLin..STYLE,;
            OrgWgh     WITH &lcPckLin..OrgStyWgh,;
            nOrdLineNo WITH &lcPckLin..nOrdLineNo,;
            SzCnt      WITH &lcPckLin..SzCnt,;
            cStatus    WITH "A",;
            Cart_No    WITH m.lnLnTo1 &&lnFrom+lnJ

          lcSizeFld   = 'Size'   + ALLTRIM(STYLEUPC.SIZE)
          lcqQtyFld   = 'Qty'+ALLTRIM(STYLEUPC.SIZE)
          lcSelFld    = 'cSelect'+ALLTRIM(STYLEUPC.SIZE)
          lcCtnQtyFld = 'CtnQty'+ALLTRIM(STYLEUPC.SIZE)
          lccSizeFld  = 'cSize'+ALLTRIM(STYLEUPC.SIZE)

          REPLACE &lcSizeFld WITH IIF(!EMPTY(&lcPckLin..&lcSelFld) AND &lcPckLin..&lcCtnQtyFld >0,&lcPckLin..&lccSizeFld,&lcSizeFld)
          REPLACE Br1        WITH !EMPTY(Size1),;
            Br2        WITH !EMPTY(Size2),;
            Br3        WITH !EMPTY(Size3),;
            Br4        WITH !EMPTY(Size4),;
            Br5        WITH !EMPTY(Size5),;
            Br6        WITH !EMPTY(Size6),;
            Br7        WITH !EMPTY(Size7),;
            Br8        WITH !EMPTY(Size8)

          REPLACE &lcqQtyFld   WITH m.lnQty &&&lcPckLin..&lcpQtyFld
          REPLACE Weight1    WITH IIF(EMPTY(&lcPckLin..cSelect1) AND EMPTY(&lcPckLin..CtnQty1),Weight1,Weight1+&lcPckLin..StyWgh*(&lcPckLin..CtnQty1)),;
            Weight2    WITH IIF(EMPTY(&lcPckLin..cSelect2) AND EMPTY(&lcPckLin..CtnQty2),Weight2,Weight2+&lcPckLin..StyWgh*(&lcPckLin..CtnQty2)),;
            Weight3    WITH IIF(EMPTY(&lcPckLin..cSelect3) AND EMPTY(&lcPckLin..CtnQty3),Weight3,Weight3+&lcPckLin..StyWgh*(&lcPckLin..CtnQty3)),;
            Weight4    WITH IIF(EMPTY(&lcPckLin..cSelect4) AND EMPTY(&lcPckLin..CtnQty4),Weight4,Weight4+&lcPckLin..StyWgh*(&lcPckLin..CtnQty4))
          REPLACE Weight5    WITH IIF(EMPTY(&lcPckLin..cSelect5) AND EMPTY(&lcPckLin..CtnQty5),Weight5,Weight5+&lcPckLin..StyWgh*(&lcPckLin..CtnQty5)),;
            Weight6    WITH IIF(EMPTY(&lcPckLin..cSelect6) AND EMPTY(&lcPckLin..CtnQty6),Weight6,Weight6+&lcPckLin..StyWgh*(&lcPckLin..CtnQty6)),;
            Weight7    WITH IIF(EMPTY(&lcPckLin..cSelect7) AND EMPTY(&lcPckLin..CtnQty7),Weight7,Weight7+&lcPckLin..StyWgh*(&lcPckLin..CtnQty7)),;
            Weight8    WITH IIF(EMPTY(&lcPckLin..cSelect8) AND EMPTY(&lcPckLin..CtnQty8),Weight8,Weight8+&lcPckLin..StyWgh*(&lcPckLin..CtnQty8))
          REPLACE cStatus    WITH IIF(&lcPckLin..SELECTED>0,'M',cStatus)

          SELECT(lcPckLin)
          REPLACE CtnQty1  WITH IIF(!llClrSel AND !EMPTY(cSelect1),&lcPckLin..OQty1,0),;
            CtnQty2  WITH IIF(!llClrSel AND !EMPTY(cSelect2),&lcPckLin..OQty2,0),;
            CtnQty3  WITH IIF(!llClrSel AND !EMPTY(cSelect3),&lcPckLin..OQty3,0),;
            CtnQty4  WITH IIF(!llClrSel AND !EMPTY(cSelect4),&lcPckLin..OQty4,0),;
            CtnQty5  WITH IIF(!llClrSel AND !EMPTY(cSelect5),&lcPckLin..OQty5,0),;
            CtnQty6  WITH IIF(!llClrSel AND !EMPTY(cSelect6),&lcPckLin..OQty6,0),;
            CtnQty7  WITH IIF(!llClrSel AND !EMPTY(cSelect7),&lcPckLin..OQty7,0),;
            CtnQty8  WITH IIF(!llClrSel AND !EMPTY(cSelect8),&lcPckLin..OQty8,0)
          REPLACE cSelect1 WITH IIF(!llClrSel AND !EMPTY(cSelect1),'»',''),;
            cSelect2 WITH IIF(!llClrSel AND !EMPTY(cSelect2),'»',''),;
            cSelect3 WITH IIF(!llClrSel AND !EMPTY(cSelect3),'»',''),;
            cSelect4 WITH IIF(!llClrSel AND !EMPTY(cSelect4),'»',''),;
            cSelect5 WITH IIF(!llClrSel AND !EMPTY(cSelect5),'»',''),;
            cSelect6 WITH IIF(!llClrSel AND !EMPTY(cSelect6),'»',''),;
            cSelect7 WITH IIF(!llClrSel AND !EMPTY(cSelect7),'»',''),;
            cSelect8 WITH IIF(!llClrSel AND !EMPTY(cSelect8),'»','')
          IF SEEK(STR(lnPackCtn,4),lcCtnHdr)
            REPLACE &lcCtnHdr..cCrtnVlTyp WITH &lcPckLin..cCrtnVlTyp
          ENDIF
          lnOldFrm = lnFrom
          lnOldTo  = lnLnTo1
          SHOW GET lnFrom
          SHOW GET lnLnTo1
          IF llPalStat
            STORE lnMaxPal + 1 TO lnCtnPal
          ENDIF
        ENDIF
        SET ORDER TO lcTag IN (lcPckLin)
      ENDIF
    ENDIF
  ENDIF
  SELECT(lcPckLin)
  =RLOCK(lcPckLin)
  UNLOCK IN (lcPckLin)
  SET RELATION TO '' INTO (lcScaFile) ADDITIVE
  SET SKIP TO (lcScaFile)

  IF lnCurRec <= RECCOUNT()
    GOTO lnCurRec
    SKIP lnSizeRec-1
  ENDIF

  SELECT (lcCtnHdr)
  GOTO TOP
  = RLOCK(lcCtnHdr)
  UNLOCK IN (lcCtnHdr)

  SELECT (lcCtnDtl)
  GOTO TOP
  = RLOCK(lcCtnDtl)
  UNLOCK IN (lcCtnDtl)

  IF llScrPrnLb
    PRIVATE lnDetRec , lnCrtRec , lnCarRef , lcCurrAlis
    *-- Save current setting
    lcCurrAlis = SELECT (0)
    lnDetRec   = RECNO(lcCtnDtl)
    lnCrtRec   = RECNO(lcCartonSz)
    lnCarRef = 0
    FOR lnCarRef = lnOldFrm TO lnOldTo
      =SEEK(STR(lnCarRef,4),lcCtnDtl) AND lfvLblInfo(lnCarRef,llScrPrnLb)
    ENDFOR

    *-- Restore Setting.
    IF BETWEEN(lnDetRec,1,RECCOUNT(lcCtnDtl))
      GO lnDetRec IN (lcCtnDtl)
    ENDIF

    IF BETWEEN(lnCrtRec,1,RECCOUNT(lcCartonSz))
      GO lnCrtRec IN (lcCartonSz)
    ENDIF

    SELECT (lcCurrAlis)
  ENDIF
  = lfwDtlBrow()
  = lfRefresh(lcWinHdr)
  SELECT (lnCurAlias)
  *-- END OF lfvApply1
  RETURN

  *!*************************************************************
  *! Function  : lfViewRLog
  *! Developer : Amin Khodary Amin
  *! Date      : 08/23/1999
  *! Purpose   : - Preview rebalance log file (ReBalLog.txt).
  *!*************************************************************
FUNCTION lfViewRLog

  lcWinTitl  = "PAKINK LIST  Report"
  STORE FSEEK(lcFilHandl, 0, 2) TO ifp_size	&& Move pointer to EOF

  =FFLUSH(lcFilHandl)
  * Close the text file
  DO WHILE !FCLOSE(lcFilHandl)

  ENDDO

  IF ifp_size <>212
    CREATE CURSOR TMPSTR (mStrRep M(10))
    APPEND BLANK
    *  APPEND MEMO mStrRep FROM (gcWorkDir+"Pick_txt.txt") OVERWRITE
    *'&gcWorkDir.'+lcPick_txt+'.txt')
    APPEND MEMO mStrRep FROM (gcWorkDir+lcPick_Txt+'.txt') OVERWRITE
    DO (gcScrDir + 'SM\SMSTRREP.SPR')
    USE IN TMPSTR
  ENDIF
  RETURN


FUNCTION lfUpdate
  = gfUpdate()
  IF UPDATED()
    llAnyUpd = .T.
  ENDIF

*C125761,2 EIH 08/31/2005 fix the problem of when print the log report [Start].
*!*************************************************************
*! Name      : lfPrnt
*! Developer : EIH
*! Date      : 
*! Purpose   : Print the log report
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************
FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*C125761,2 EIH 08/31/2005 [End].