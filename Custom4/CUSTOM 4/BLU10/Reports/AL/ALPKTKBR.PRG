*:************************************************************************
*:*: Procedure file: ALPKTKBR.PRG
*:*:         System: ARIA 4XP
*:         Module: Allocation
*:         Author: Mariam Mazhar[MMT]
*:      Copyright (c) 
*:  Last modified: 02/01/2007
*:  C200739 T20070108.0124 
*:  Procs & Fncts: lpCreaTemp
*:               : lfLineCond
*:               : lfNewLine
*:               : lfSlSpAdr
*:************************************************************************
SELECT (lcTmpOrdL)
LOCATE
*-- Check if there is no record then return 
IF EOF()
  RETURN
ENDIF
lcTmpOrdLn =gfTempName()
lcTmpInd = ORDER()
INDEX ON PikTkt + Order +style + STR(LineNo , 6) TAG (lcTmpOrdLn)

loogScroll.cCROrientation = 'L'

DECLARE laItemSeg[1]
STORE 0 TO lnsclLen,lnSclPos           && SYREPUVR
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnsclLen = LEN(laItemSeg[lnCount,3])
    lnSclPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--Variable section
lnTotqty = 0                            && Variable to get total quantity
lnTotpik = 0                            && Variable to get total piked tiket
llfunctn=lfSolSpAdr()                   && var to call function lfSolSpAdr to carry at frx
*--End variable section
*-- lnScaleLen : Scale Header Length (Defined in Syrepuvr)

lnScaleLen = 3-lnScaleLen
=lpCreaTemp()                          && Creating temp file lcSclTmp (Defined in Syrepuvr)

lnActiveSz = 0                         && Variable to check 14 size 
SELECT (lcTmpordL)
SCAN
  =lfLineCond()                        && Add/Update line(s)
ENDSCAN

SELECT (lcTmpOrdL)
SET ORDER TO (lcTmpInd)
SET RELATION OFF INTO &lcStyleFile
SET RELATION OFF INTO &lcPiktktTemp 
SET RELATION OFF INTO &lcOrdLnTmp 

SELECT (lcSclTmp)
SET ORDER TO (lcSclTmp) ASCENDING
SET RELATION TO PikTkt + Order INTO (lcTmpOrdL)   && Setting relation between our temp.
SET RELATION TO Style INTO &lcStyleFile ADDITIVE
SET RELATION TO PikTkt INTO &lcPiktktTemp  ADDITIVE

IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO &lcOrdLnTmp  ADDITIVE
ENDIF   
SELECT (lcSclTmp)

*!*****************************************************************************
*! Name      : lfLineCond
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/01/2007
*! Purpose   : Function To Collect Data for ext Size Scale to get 14 size
*!           : at current record
*!*****************************************************************************
*! Example   : =lfColctDat
*!*****************************************************************************
*!* This function is made to check extended size scale for collecting 14 size that
*!* has the same scale group and style but differant scale size .
*!*
*!*******************************************************
*!
FUNCTION lfLineCond

IF SEEK(PIKTKT+ Order + cGrupDetal,lcSclTmp)
  lnalias=ALIAS()
  SELECT (lcSclTmp)
  LOCATE REST WHILE PIKTKT+ORDER+CGRUPDETAL=&lcTmpordL..PIKTKT+&lcTmpordL..ORDER+&lcTmpordL..CGRUPDETAL;
               FOR LEFT(SCALE,lnScaleLen)=LEFT(&lcTmpordL..SCALE,lnScaleLen).AND. ;
               LEFT(STYLE,LEN(ALLTRIM(STYLE))-3) = LEFT(&lcTmpOrdL..STYLE,LEN(ALLTRIM(STYLE))-3) .AND. nLastSz < 14
  IF FOUND()         
      *-- if it is the same scale size
      SELECT(lcTmpOrdL)             
      IF RIGHT(SCALE,3-lnScaleLen) = RIGHT(&lcSclTmp..SCALE,3-lnScaleLen)
        lnActiveSz = 0
        lnTotpik = 0 
        lnTotqty = 0
      ELSE                     && else If not the same scale continue adding sizes
        lnActiveSz = &lcSclTmp..nLastSz
        lnTotpik = &lcSclTmp..Totpik
        lnTotqty = &lcSclTmp..Totqty
      
        *-- if you reach to 14 sizes then add new record
        IF lnActiveSz = 14                                 
           REPLACE &lcSclTmp..Totqty WITH lnTotQty,;
                   &lcSclTmp..TotPik WITH lnTotPik
  
          lnActiveSz = 0
          lnTotpik = 0 
          lnTotqty = 0
        ENDIF                      && End if you reach to 14 sizes then add new record

      ENDIF                        && End if it is the same scale size

    ELSE                      && else If not the same style add new record
  
      lnActiveSz  = 0
      lnTotpik = 0 
      lnTotqty = 0
  ENDIF                          && End if it is the same style
ELSE                        && else If not the same Group scale add new record
    lnActiveSz = 0
    lnTotpik = 0 
    lnTotqty = 0
ENDIF                             && End if it is the same scale group

SELECT (lcTmpOrdL)
SCATTER MEMVAR MEMO
IF (lnActiveSz = 0)
  =lfNewLine(.T.)                 && Insert New Record
ELSE
  =lfNewLine()                    && Update and/or insert record
ENDIF
REPLACE &lcSclTmp..Totqty WITH lnTotQty,;
        &lcSclTmp..TotPik WITH lnTotPik
*!*****************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/01/2007
*! Purpose   : Function  To Create Temp File for ext. size Scale to get 14 size
*!*****************************************************************************
*! Example   : =lpCreaTemp()
*!*****************************************************************************
*!* 
FUNCTION lpCreaTemp
PRIVATE lcFullPath , lnRows

IF USED(lcSclTmp)
  USE IN (lcSclTmp)
ENDIF
IF !USED(lcSclTmp)
  DIMENSION laTempStru[1,4]
  SELECT (lcTmpOrdL)
  lnRows=AFIELDS(laTempStru)
  lnOldLength =lnRows
  DIMENSION laTempStru[lnRows + 27 , 18]

FOR lnI = 1 TO 6
  laTempStru[lnI + lnRows ,1] = "Qty"+ALLTRIM(STR(lnI+8))
  laTempStru[lnI + lnRows ,2] = "N"
  laTempStru[lnI + lnRows ,3] = 6
  laTempStru[lnI + lnRows ,4] = 0
ENDFOR
lnRows = lnRows + 6

FOR lnI = 1 TO 6
  laTempStru[lnI + lnRows ,1] = "Pik"+ALLTRIM(STR(lnI+8))
  laTempStru[lnI + lnRows ,2] = "N"
  laTempStru[lnI + lnRows ,3] = 6
  laTempStru[lnI + lnRows ,4] = 0
ENDFOR
lnRows = lnRows + 6

FOR lnI = 1 TO 14
  laTempStru[lnI + lnRows ,1] = "Sz"+ALLTRIM(STR(lnI))
  laTempStru[lnI + lnRows ,2] = "C"
  laTempStru[lnI + lnRows ,3] = 5
  laTempStru[lnI + lnRows ,4] = 0
ENDFOR
lnRows = lnRows + 14

laTempStru[lnRows + 1 ,1] = "NLASTSZ"
laTempStru[lnRows + 1 ,2] = "N"
laTempStru[lnRows + 1 ,3] = 2
laTempStru[lnRows + 1 ,4] = 0


FOR LOOP =1 TO 27
  STORE ' ' TO  laTempStru[lnOldLength +Loop,7],laTempStru[lnOldLength +Loop,8],;
                laTempStru[lnOldLength +Loop,9],laTempStru[lnOldLength +Loop,10],;
                laTempStru[lnOldLength +Loop,11],laTempStru[lnOldLength +Loop,12],;
                laTempStru[lnOldLength +Loop,13],laTempStru[lnOldLength +Loop,14],;
                laTempStru[lnOldLength +Loop,15],laTempStru[lnOldLength +Loop,16]
  STORE 0 TO    laTempStru[lnOldLength +Loop,17] ,laTempStru[lnOldLength +Loop,18]
ENDFOR   



= gfCrtTmp(lcSclTmp ,@laTempStru,'PIKTKT+ Order + cGrupDetal+STR(LineNo , 6)+ LEFT(SCALE,lnScaleLen)',lcSclTmp )

SELECT (lcSclTmp )
INDEX ON PIKTKT+ Order + cGrupDetal+STR(LineNo , 6)+LEFT(SCALE,lnScaleLen) TAG (lcSclTmp) 
ENDIF

*!*****************************************************************************
*! Name      : lfNewLine
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/01/2007
*! Purpose   : Function To Insert new record for ext Size Scale to get 14 size
*!           : or to continue to collect 14 size at the same record in temp file 
*!*****************************************************************************
*! Parameters: llInsert -> passes true to insert record at first time
*!*****************************************************************************
*! Example   : =lfNewLine
*!*****************************************************************************
*!*
FUNCTION lfNewLine
PARAMETERS llInsert
PRIVATE lnI
IF &lcTmpOrdL..cGrupDetal = "H"                    && To insert header record
  INSERT INTO (lcSclTmp) FROM MEMVAR  
ELSE
  IF llInsert                                      && Check if we add new record or not
    INSERT INTO (lcSclTmp) FROM MEMVAR
  ENDIF  
  *-- loop to get all piks, qtys and sizes 
  =SEEK('S'+&lcTmpOrdL..Scale,lcScaleFile)
  FOR lnI = 1 TO &lcScaleFile..CNT
    lcI = STR(lnI,1)
    IF !EMPTY(&lcTmpOrdL..Qty&lcI)                             && Check if there is any empty qty to ignor
      lnActiveSz = lnActiveSz + 1
      IF lnActiveSz = 15                           && Check if it reach 14 size to add new and update total qty and total piks
         REPLACE &lcSclTmp..Totqty WITH lnTotQty,;
                 &lcSclTmp..TotPik WITH lnTotPik
        INSERT INTO (lcSclTmp) FROM MEMVAR
        lnActiveSz = 1
        
        lnTotpik = 0
        lnTotqty = 0 
      ENDIF

      lcActSz = ALLTRIM(STR(lnActiveSz))
      REPLACE &lcSclTmp..Qty&lcActSz WITH Qty&lcI     ,;
              &lcSclTmp..Pik&lcActSz WITH Pik&lcI     ,;
              &lcSclTmp..Sz&lcActSz  WITH &lcScaleFile..SZ&lcI,;
              &lcSclTmp..NLASTSZ     WITH lnActiveSz

      lnTotpik = lnTotpik + Pik&lcI             && To get total piked
      lnTotqty = lnTotqty + Qty&lcI             && To get Total quantity
    ENDIF
  ENDFOR
ENDIF
*-- End of lfNewLine 
*!*****************************************************************************
*! Name      : lfSlSpAdr
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/01/2007
*! Purpose   : Function To call lfSolSpAdr and select local temp. 
*!*****************************************************************************
*! Example   : lfSlSpAdr
*!*****************************************************************************
*!*
FUNCTION lfSlSpAdr
PARAMETER lcDummy
PRIVATE lcDummy , lnActvAlis
lnActvAlis = SELECT(0)
=lfSolSpAdr()                                     && File and the tmp. file comming from main prg
SELECT (lnActvAlis)
RETURN ''
