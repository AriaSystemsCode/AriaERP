*:************************************************************************
*:
*: Procedure file: ALPKTKBR.PRG
*:
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: Mohamed Shokry
*:      Copyright (c) 
*:  Last modified: 25/5/2000
*:
*:  Procs & Fncts: lpCreaTemp
*:               : lfLineCond
*:               : lfNewLine
*:               : lfSlSpAdr
*:  Documented    7/6/2000
*:************************************************************************
*C101719, This program was coded by MHM for Customer Blue B.
*:***************************************************************************
*: Modifications:
*B803867,1 AAN 01/03/2001 1-Fix bug that not print item header info
*B803867,1                2-Remove allocated line   
*B803867,1                3-move all information closer to top of form
*B803867,1                4-Change to 8 1/2x11 paper not A4 (Done in the .FRX)
*:***************************************************************************
SELECT (lcTmpOrdL)
LOCATE
*-- Check if there is no record then return 
IF EOF()
  RETURN
ENDIF
lcTmpOrdLn =gfTempName()
lcTmpInd = ORDER()
INDEX ON PikTkt + Order +style + STR(LineNo , 6) TAG (lcTmpOrdLn)
*--The scale length to get style without scale.
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
*--MHM TO GET TRUEE SCALE LENGTH
lnScaleLen = 3-lnScaleLen
*--MHM
=lpCreaTemp()                          && Creating temp file lcSclTmp (Defined in Syrepuvr)

*----- Data Collection
lnActiveSz = 0                         && Variable to check 14 size 
SELECT (lcTmpordL)
SCAN
  =lfLineCond()                        && Add/Update line(s)
ENDSCAN
*-- End of ALPKTKBR.

*-- To refrisfh the relations of style and lcTmpOrdL
SELECT (lcTmpOrdL)
SET ORDER TO (lcTmpInd)
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO PIKTKT
*B803867 AAN Remove a relation[Begin].
SET RELATION OFF INTO ORDLINE
*B803867 AAN Remove a relation[End].
SELECT (lcSclTmp)
SET ORDER TO (lcSclTmp) ASCENDING
SET RELATION TO PikTkt + Order INTO (lcTmpOrdL)   && Setting relation between our temp.
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
*B803867 AAN Add relation when printing Order Line Notepad[Begin].
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF   
*803867 AAN Add relation when printing Order Line Notepad[End].
SELECT (lcSclTmp)
*-- end of report code.
*!*****************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mohamed Shokry
*! Date      : 25/05/2000
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
    DIMENSION laTempStru[lnRows + 27 , 4]

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

  CREATE CURSOR (lcSclTmp) FROM ARRAY laTempStru
  ZAP
  lcFullPath = SET("FULLPATH")                   && To get path of temp file  and use it to create index
  SET FULLPATH ON
  lcCDXName  = STRTRAN(DBF() , ".TMP" , ".CDX")

*B803867 AAN Add field cGrupDetal to the second index and STR(LineNo , 6) to the first index[Begin].
  *INDEX ON PIKTKT+ Order + cGrupDetal+ LEFT(SCALE,lnScaleLen) TAG (lcSclTmp)
  *-- set index on PIKTKT+ Order + LEFT(SCALE,lnScaleLen) 
  *INDEX ON PIKTKT+ Order + LEFT(SCALE,lnScaleLen);
	   TAG (lcSclTmp) OF (lcCDXName) DESC

  INDEX ON PIKTKT+ Order + cGrupDetal+STR(LineNo , 6)+ LEFT(SCALE,lnScaleLen) TAG (lcSclTmp)
  *-- set index on PIKTKT+ Order + LEFT(SCALE,lnScaleLen) 
  INDEX ON PIKTKT+ Order + cGrupDetal+STR(LineNo , 6)+LEFT(SCALE,lnScaleLen);
	   TAG (lcSclTmp) OF (lcCDXName) 
*B803867 AAN Add field cGrupDetal to the second index and STR(LineNo , 6) to the first index[End].
ENDIF

*-- End of lpCreaTemp.
*!*****************************************************************************
*! Name      : lfLineCond
*! Developer : Mohamed Shokry
*! Date      : 06/06/2000
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

*-- if it is the same scale group
*B803867 AAN Change seek cond. and add locate stat. [Begin].
*IF SEEK(PIKTKT + ORDER+LEFT(SCALE,lnScaleLen),"&lcSclTmp")
IF SEEK(PIKTKT+ Order + cGrupDetal,"&lcSclTmp")
  lnalias=ALIAS()
  SELECT (lcSclTmp)
  LOCATE REST WHILE PIKTKT+ORDER+CGRUPDETAL=&lcTmpordL..PIKTKT+&lcTmpordL..ORDER+&lcTmpordL..CGRUPDETAL;
               FOR LEFT(SCALE,lnScaleLen)=LEFT(&lcTmpordL..SCALE,lnScaleLen).AND. ;
               LEFT(STYLE,LEN(ALLTRIM(STYLE))-3) = LEFT(&lcTmpOrdL..STYLE,LEN(ALLTRIM(STYLE))-3) .AND. nLastSz < 14
  IF FOUND()         
*B803867 AAN Change seek cond. and add locate stat. [End].  
    *-- if it is the same style
    *B803867,1 AAN Move it up in the locate condition
    *IF PADR(STYLE,LEN(ALLTRIM(STYLE))-3) = PADR(&lcSclTmp..STYLE,LEN(ALLTRIM(STYLE))-3)
    *B803867,1 AAN end
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
  *B803867,1 AAN comment out.
  *ENDIF
  *B803867,1 AAN end
ENDIF                             && End if it is the same scale group

*B803867,1 AAN ensure that you are on the right file before scattering
SELECT (lcTmpOrdL)
*B803867,1 AAN end
SCATTER MEMVAR MEMO
IF (lnActiveSz = 0)
  =lfNewLine(.T.)                 && Insert New Record
ELSE
  =lfNewLine()                    && Update and/or insert record
  *   REPLACE &lcSclTmp..Totqty WITH lnTotQty,;
                 &lcSclTmp..TotPik WITH lnTotPik
ENDIF
REPLACE &lcSclTmp..Totqty WITH lnTotQty,;
        &lcSclTmp..TotPik WITH lnTotPik
*-- End of lfLineCond

*!*****************************************************************************
*! Name      : lfNewLine
*! Developer : Mohamed Shokry
*! Date      : 06/06/2000
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
  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    *B803867,1 AAN ensure the right table 
    *IF !EMPTY(Qty&lcI)                             && Check if there is any empty qty to ignor
    IF !EMPTY(&lcTmpOrdL..Qty&lcI)                             && Check if there is any empty qty to ignor
    *B803867,1 AAN end
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
              &lcSclTmp..Sz&lcActSz  WITH SCALE.SZ&lcI,;
              &lcSclTmp..NLASTSZ     WITH lnActiveSz

      lnTotpik = lnTotpik + Pik&lcI             && To get total piked
      lnTotqty = lnTotqty + Qty&lcI             && To get Total quantity

    ENDIF
  ENDFOR
ENDIF
*-- End of lfNewLine 
       
*!*****************************************************************************
*! Name      : lfSlSpAdr
*! Developer : Mohamed Shokry
*! Date      : 03/06/2000
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
*----End lfSlSpAdr
