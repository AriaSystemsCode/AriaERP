*:***************************************************************************
*: Program file  : ICSLCRT
*: Program desc. : Scales Report
*: For Report    : (ICSLCRTE.FRX,ICSLCRTN.FRX)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mohamed Atia Badran (MAB)
*: Date          : 01/11/1999   
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfModalGen,gfRange,lfwOgWhen,lfGetForm.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSLCRT
*:***************************************************************************
*: This Report Program is due to E301117 ...
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*
lcStTime = TIME()
DIMENSION laPrePaks[1,1]

*-- N.R.F. Expression
lcNrfExp = [!(EMPTY(cNRFCode1) AND EMPTY(cNRFCode2) AND EMPTY(cNRFCode3) AND EMPTY(cNRFCode4) AND EMPTY(cNRFCode5) AND EMPTY(cNRFCode6) AND EMPTY(cNRFCode7) AND EMPTY(cNRFCode8))]

SELECT SCALE
*-- Create temp. file have scales and prepacks. [Begin]
lcTempScl = gfTempName()
DIMENSION laStruArr[1,4]
=AFIELDS(laStruArr)

CREATE CURSOR (lcTempScl) FROM ARRAY laStruArr
SELECT (lcTempScl)
ZAP
INDEX ON TYPE + SCALE + PREPAK TAG (lcTempScl) OF (gcWorkDir+lcTempScl+'.CDX')
*-- Create temp. file have scales and prepacks. [End  ]

SELECT SCALE
*-- Make report expression (Optimized expression) [Begin
IF lcRpExp = '.T.'
  lcRpExp = [type+scale+prepak = '']
ENDIF  

*-- if Extended size scale
IF llExtSize
  SET RELATION TO LEFT(SCALE,lnExtWidth) INTO SCALEHD
ENDIF

lcRpExp = STRTRAN(lcRpExp,'SCALE.','')

*-- Make report expression (Optimized expression) [End..

*-- Loop Scale file to add filtered lines. [Begin
SCAN FOR &lcRpExp AND TYPE='S'
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTempScl) FROM MEMVAR
ENDSCAN

*-- Loop Scale file to add filtered lines. [End..
SELECT (lcTempScl)
IF RECCOUNT(lcTempScl) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

IF llRpPrepak OR llRpNRF
  SCAN FOR TYPE = 'S'
    
    lcCurrRec = 'S' + SCALE
    IF llRpPrepak AND SEEK('P'+SCALE,'SCALE')
      SELECT SCALE
      SCAN FOR TYPE+SCALE+PREPAK = 'P' + &lcTempScl..SCALE
        
        IF pPTot > 0
          SCATTER MEMVAR MEMO
          m.Cnt = &lcTempScl..Cnt
          INSERT INTO (lcTempScl) FROM MEMVAR
        ENDIF  
        
      ENDSCAN

      =SEEK(lcCurrRec,lcTempScl)
    ENDIF

    SELECT(lcTempScl)
    
    IF llRpNRF AND EVALUATE(lcNrfExp)    
      SCATTER MEMVAR MEMO
      m.Type      = 'N'
      m.Prepak    = 'Z'
      INSERT INTO (lcTempScl) FROM MEMVAR
      =SEEK(lcCurrRec,lcTempScl)
    ENDIF
  
  ENDSCAN

ENDIF

IF llExtSize
  INDEX ON LEFT(SCALE,lnExtWidth) + PREPAK TAG (lcTempScl) OF (gcWorkDir+lcTempScl+'.CDX')
  *-- Make relation with header file.
  SELECT SCALE
  SET RELATION TO
  SELECT (lcTempScl)
  SET RELATION TO LEFT(SCALE,lnExtWidth) INTO SCALEHD
ELSE
  INDEX ON SCALE + PREPAK TAG (lcTempScl) OF (gcWorkDir+lcTempScl+'.CDX')
ENDIF

STORE .T. TO llPackFlag,llNrfFlag
llHavePp   = .F.
lcPrepak   = ''
lcCurrType = ' '

DO gfDispRe WITH EVAL('lcRpForm')  && Do report.
USE IN (lcTempScl)
*-- end of program code.

*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 01/11/1999
*! Purpose   : OG when function
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************
*!
FUNCTION lfwOgWhen
SET ORDER TO TAG SCALE IN SCALE
SET ORDER TO TAG Extscale IN SCALEHD
*-- end of lfwOgWhen.

*!*************************************************************
*! Name      : lfPackFlag
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 01/11/1999
*! Purpose   : Rising Prepak flag.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfPackFlag()
*!*************************************************************
*!
FUNCTION lfPackFlag
STORE (Type = 'S') TO llPackFlag,llNrfFlag
RETURN ''
*-- end of lfPackFlag.

*!*************************************************************
*! Name      : lfHavePp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 01/11/1999
*! Purpose   : Rising Prepak flag.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfHavePp()
*!*************************************************************
*!
FUNCTION lfHavePp
llHavePp = .F.
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  IF pP&lcI > 0
    llHavePp = .T.
    EXIT
  ENDIF
ENDFOR
RETURN llHavePp
*-- end of lfHavePp.

*!*************************************************************
*! Name      : lfInitVal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 01/11/1999
*! Purpose   : Initial prepak value.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfInitVal()
*!*************************************************************
*!
FUNCTION lfInitVal
lcPrepak = ''
RETURN lcPrepak
*-- end of lfInitVal.

*!*************************************************************
*! Name      : lfCurrVal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 01/11/1999
*! Purpose   : Current prepak value.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfInitVal()
*!*************************************************************
*!
FUNCTION lfCurrVal
lcPrepak = PREPAK
RETURN ''
*-- end of lfCurrVal.

*FUNCTION lfWhere
*RETURN SCALE
*-- end of lfWhere.

*!*************************************************************
*! Name      : lfFilMmVar
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/14/1999
*! Purpose   : Fill Memory variables.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfFilMmVar()
*!*************************************************************
*!
FUNCTION lfFilMmVar
PRIVATE laSetups
DIMENSION laSetups[2,2]
laSetups[1,1] = 'M_USEEXSSC'
laSetups[2,1] = 'M_EXTWIDTH'
=gfGetMemVar(@laSetups)
llExtSize  = laSetups[1,2]
lnExtWidth = laSetups[2,2]
lcRpForm   = IIF(llExtSize,'ICSCLRTE','ICSCLRTN')
*-- end of lfFilMmVar.

*!*************************************************************
*! Name      : lfScalHd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/14/1999
*! Purpose   : Scale Header extended scale case.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfScalHd()
*!*************************************************************
*!
FUNCTION lfScalHd
PRIVATE lcScaleHd
IF ScaleHd.nnoofdim = 1
  lcScaleHd = 'One Dimension '
ELSE
  lcScaleHd = IIF(ScaleHd.nnoofdim=2,'Two ','Three ') + 'Dimensions '
ENDIF
RETURN lcScaleHd
*-- end of lfScalHd.

*!*************************************************************
*! Name      : lfCurrType
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/14/1999
*! Purpose   : Save current type
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfCurrType()
*!*************************************************************
*!
FUNCTION lfCurrType
lcCurrType = Type
RETURN ''
*-- end of lfCurrType.

*!*************************************************************
*! Name      : lfNRFLine
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/14/1999
*! Purpose   : Return NRF codes.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : NRF Line
*!*************************************************************
*! Example     : =lfNRFLine()
*!*************************************************************
*!
FUNCTION lfNRFLine
PRIVATE lcNRFLine
IF llExtSize
  lcNRFLine = SPACE(3) + RIGHT(SCALE,(3-lnExtWidth))+SPACE(19)+;
              PADL(ALLTRIM(cNRFCode1),5)+' '+PADL(ALLTRIM(cNRFCode2),5)+' '+;
              PADL(ALLTRIM(cNRFCode3),5)+' '+PADL(ALLTRIM(cNRFCode4),5)+' '+;
              PADL(ALLTRIM(cNRFCode5),5)+' '+PADL(ALLTRIM(cNRFCode6),5)+' '+;
              PADL(ALLTRIM(cNRFCode7),5)+' '+PADL(ALLTRIM(cNRFCode8),5)
ELSE
  lcNRFLine = 'N.R.F.:  ' + SPACE(24)+;
              PADL(ALLTRIM(cNRFCode1),5)+' '+PADL(ALLTRIM(cNRFCode2),5)+' '+;
              PADL(ALLTRIM(cNRFCode3),5)+' '+PADL(ALLTRIM(cNRFCode4),5)+' '+;
              PADL(ALLTRIM(cNRFCode5),5)+' '+PADL(ALLTRIM(cNRFCode6),5)+' '+;
              PADL(ALLTRIM(cNRFCode7),5)+' '+PADL(ALLTRIM(cNRFCode8),5)
ENDIF              
RETURN lcNRFLine
*-- end of lfNRFLine.

*!*************************************************************
*! Name      : lfSScale
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/14/1999
*! Purpose   : Scale In Range set function.
*!*************************************************************
*! Passed Parameters : Dummy Parameter for set only.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfSScale()
*!*************************************************************
*!
FUNCTION lfSScale
PARAMETERS lcDummyPrm
IF llExtSize
  GO TOP IN SCALEHD
ELSE
  GO TOP IN SCALE
ENDIF
*-- end of lfSScale.