*:************************************************************************
*: Program file  : apchkpgp.prg
*: Program desc. : PRINTED CHECK REPORT FORM GP
*: System        : Aria Advantage Series VER. 4XP
*: Module        : AP
*: Developer     : Mariam Mazhar (MMT)
*: Date          : 02/14/2013
*: Reference     : C201553[T20121127.0010]
*:************************************************************************
FUNCTION gffig1
LPARAMETERS lcDumm
LCTOTAL = PADL(ALLTRIM(STR(LNTOTAL,9,2)),9,'0')
LCCHAR = SUBSTR(LCTOTAL,1,1)
lcDumm =  ' '
DO CASE
   CASE LCCHAR='0'
      lcDumm =  '**ZERO**'
   CASE LCCHAR='1'
      lcDumm = '***ONE**'
   CASE LCCHAR='2'
      lcDumm = '***TWO**'
   CASE LCCHAR='3'
      lcDumm =  '*THREE**'
   CASE LCCHAR='4'
      lcDumm =  '**FOUR**'
   CASE LCCHAR='5'
      lcDumm =  '**FIVE**'
   CASE LCCHAR='6'
      lcDumm = '***SIX**'
   CASE LCCHAR='7'
      lcDumm = '*SEVEN**'
   CASE LCCHAR='8'
      lcDumm =  '*EIGHT**'
   CASE LCCHAR='9'
      lcDumm =  '**NINE**'
ENDCASE
RETURN lcDumm 

FUNCTION gffig2
LPARAMETERS lcDumm
LCTOTAL = PADL(ALLTRIM(STR(LNTOTAL,9,2)),9,'0')
LCCHAR = SUBSTR(LCTOTAL,2,1)
lcDumm = ' '
DO CASE
   CASE LCCHAR='0'
      lcDumm = '**ZERO**'
   CASE LCCHAR='1'
      lcDumm = '***ONE**'
   CASE LCCHAR='2'
      lcDumm = '***TWO**'
   CASE LCCHAR='3'
      lcDumm = '*THREE**'
   CASE LCCHAR='4'
      lcDumm =  '**FOUR**'
   CASE LCCHAR='5'
      lcDumm =  '**FIVE**'
   CASE LCCHAR='6'
      lcDumm = '***SIX**'
   CASE LCCHAR='7'
      lcDumm = '*SEVEN**'
   CASE LCCHAR='8'
      lcDumm = '*EIGHT**'
   CASE LCCHAR='9'
      lcDumm = '**NINE**'
ENDCASE
RETURN lcDumm
*
FUNCTION gffig3
LPARAMETERS lcDumm
LCTOTAL = PADL(ALLTRIM(STR(LNTOTAL,9,2)),9,'0')
LCCHAR = SUBSTR(LCTOTAL,3,1)
lcDumm = ' ' 
DO CASE
   CASE LCCHAR='0'
      lcDumm ='**ZERO**'
   CASE LCCHAR='1'
      lcDumm ='***ONE**'
   CASE LCCHAR='2'
      lcDumm ='***TWO**'
   CASE LCCHAR='3'
      lcDumm ='*THREE**'
   CASE LCCHAR='4'
      lcDumm ='**FOUR**'
   CASE LCCHAR='5'
      lcDumm ='**FIVE**'
   CASE LCCHAR='6'
      lcDumm ='***SIX**'
   CASE LCCHAR='7'
      lcDumm ='*SEVEN**'
   CASE LCCHAR='8'
      lcDumm ='*EIGHT**'
   CASE LCCHAR='9'
      lcDumm ='**NINE**'
ENDCASE
RETURN lcDumm 

FUNCTION gffig4
LPARAMETERS lcDumm 
LCTOTAL = PADL(ALLTRIM(STR(LNTOTAL,9,2)),9,'0')
LCCHAR = SUBSTR(LCTOTAL,4,1)
lcDumm =' '
DO CASE
   CASE LCCHAR='0'
      lcDumm ='**ZERO**'
   CASE LCCHAR='1'
      lcDumm ='***ONE**'
   CASE LCCHAR='2'
      lcDumm ='***TWO**'
   CASE LCCHAR='3'
      lcDumm ='*THREE**'
   CASE LCCHAR='4'
      lcDumm ='**FOUR**'
   CASE LCCHAR='5'
      lcDumm ='**FIVE**'
   CASE LCCHAR='6'
      lcDumm ='***SIX**'
   CASE LCCHAR='7'
      lcDumm ='*SEVEN**'
   CASE LCCHAR='8'
      lcDumm ='*EIGHT**'
   CASE LCCHAR='9'
      lcDumm ='**NINE**'
ENDCASE
RETURN lcDumm 
*

FUNCTION gffig5
LPARAMETERS lcDumm 
LCTOTAL = PADL(ALLTRIM(STR(LNTOTAL,9,2)),9,'0')
LCCHAR = SUBSTR(LCTOTAL,5,1)
lcDumm = ' '
DO CASE
   CASE LCCHAR='0'
      lcDumm ='**ZERO**'
   CASE LCCHAR='1'
      lcDumm ='***ONE**'
   CASE LCCHAR='2'
      lcDumm ='***TWO**'
   CASE LCCHAR='3'
      lcDumm ='*THREE**'
   CASE LCCHAR='4'
      lcDumm ='**FOUR**'
   CASE LCCHAR='5'
      lcDumm ='**FIVE**'
   CASE LCCHAR='6'
      lcDumm ='***SIX**'
   CASE LCCHAR='7'
      lcDumm ='*SEVEN**'
   CASE LCCHAR='8'
      lcDumm ='*EIGHT**'
   CASE LCCHAR='9'
      lcDumm ='**NINE**'
ENDCASE
RETURN lcDumm 

FUNCTION gffig6
LPARAMETERS lcDumm 
LCTOTAL = PADL(ALLTRIM(STR(LNTOTAL,9,2)),9,'0')
LCCHAR = SUBSTR(LCTOTAL,6,1)
lcDumm = ' '
DO CASE
   CASE LCCHAR='0'
      lcDumm ='**ZERO**'
   CASE LCCHAR='1'
      lcDumm ='***ONE**'
   CASE LCCHAR='2'
      lcDumm ='***TWO**'
   CASE LCCHAR='3'
      lcDumm ='*THREE**'
   CASE LCCHAR='4'
      lcDumm ='**FOUR**'
   CASE LCCHAR='5'
      lcDumm ='**FIVE**'
   CASE LCCHAR='6'
      lcDumm ='***SIX**'
   CASE LCCHAR='7'
      lcDumm ='*SEVEN**'
   CASE LCCHAR='8'
      lcDumm ='*EIGHT**'
   CASE LCCHAR='9'
      lcDumm ='**NINE**'
ENDCASE
RETURN lcDumm 

FUNCTION gffig7
LPARAMETERS lcDumm 
LCTOTAL = PADL(STR(LNTOTAL,9,2),9,'0')
LCCHAR = SUBSTR(LCTOTAL,8,2)
lcDumm ='**'+LCCHAR+'**'
RETURN lcDumm 
*
