*:**************************************************************************
*: Program file  : MAFXFBD
*: Program desc. : Fix program to add missing main record of the warehouse
*: System        : Aria Advantage Series.
*: Module        : Materials (MA)
*: Developer     : AHMED MAHER (AMH)
*: Date          : 09/23/2002
*: Reference     : B606497,4
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*

IF !gfSetup()
  RETURN
ENDIF

DEFINE WINDOW (gcBaseWind) FROM 0,0 TO 1,1
ACTIVATE WINDOW (gcBaseWind)

SELECT FABDYE
LOCATE
lcKeyVal = SPACE(19)
SCAN FOR FABRIC+COLOR+CWARECODE > lcKeyVal .AND. !EMPTY(DYELOT)
  lcKeyVal = FABRIC+COLOR+CWARECODE
  WAIT WINDOW 'Fixing ...' + CHR(13) + CHR(10) +;
              'Fabric : ' + FABRIC + CHR(13) + CHR(10) +;
              'Color : ' + COLOR + CHR(13) + CHR(10) +;
              'Warehouse : ' + CWARECODE + CHR(13) + CHR(10) NOWAIT
  IF !SEEK(lcKeyVal+SPACE(10))
    APPEND BLANK
    REPLACE FABRIC    WITH SUBSTR(lcKeyVal,1,7),;
            COLOR     WITH SUBSTR(lcKeyVal,8,6),;
            CWARECODE WITH SUBSTR(lcKeyVal,14,6)
  ENDIF
ENDSCAN
WAIT CLEAR
SELECT MATINVJL
DELETE ALL
SELECT ROLLS
DELETE ALL
=gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'Fix FABDYE file completed successfully.')
=gfEscap()