PROCEDURE SCXFormSet
LPARAMETERS f
LOCAL lcProp, lcMeth, lnRec
LOCAL ARRAY a[1]
LOCAL ARRAY b[1,1]
  SET DELETED ON
  IF TYPE('f')<>'C'
    CLOSE DATABASES
    SET CPDIALOG OFF
    dir='r:\a4xpdemo\aria4xp\screens'
    RELEASE lnCnt
    PUBLIC lnCnt, lcS
    lnCnt=0
    lcS=''
    DO SCXFormSet WITH dir
    n=adir(a,dir+'\*. ','D')
    FOR c=1 TO n
      IF a[c,1]='.' or a[c,1]='..'
        loop
      ENDIF
      do SCXFormSet with dir+'\'+a[c,1]
    NEXT
  ELSE
    nn=adir(b,f+'\*.scx')
    nn=ALEN(b,1)
    IF TYPE('b[1]')<>'C'
      RETURN
    ENDIF
    SET PATH TO r:\aria4xp\dbfs\01;r:\a4xpdemo\aria4xp\sysfiles;r:\a4xpdemo\aria4xp\classes
    FOR cc=1 TO nn

      IF !FILE(f+'\'+STRTRAN(LOWER(b[cc,1]),'.scx','.sct'))
        WAIT WINDOW f+'\'+STRTRAN(LOWER(b[cc,1]),'.scx','.sct')+' Not Found'
        LOOP
      ENDIF
      USE (f+'\'+b[cc,1]) EXCLUSIVE

      IF !EMPTY(DBF())

        LOCATE ALL FOR LOWER(left(baseclass,10))='formset' AND LOWER(LEFT(class,12))='ariaformset'

        IF !EOF()
          lcProp = properties
          lcMeth  = Methods
          IF 'WindowType = 1'+CHR(13)+CHR(10) $ lcProp && Branched Screen
            REPLACE class WITH 'ariabranchformset'
            ChangeText(@lcProp,'BorderStyle =','BorderStyle = 2')
            ChangeText(@lcProp,'MinButton =','MinButton = .F.')
            ChangeText(@lcProp,'MaxButton =','MaxButton = .F.')
            ChangeText(@lcProp,'Desktop =','Desktop = .T.')
            ChangeText(@lcProp,'ShowWindow =','ShowWindow = 1')
            REPLACE properties WITH lcProp
          ELSE && Non-Branched Screen
            SET SAFETY OFF
            lnRec = RECNO()
            SCATTER MEMVAR memo
            COPY all for RECNO()<=lnRec to t
            COPY all for RECNO()>lnRec to tt
            ZAP
            APPEND FROM t
            APPEND BLANK
            GATHER MEMVAR MEMO
            SET SAFETY ON
            REPLACE UniqueID WITH SYS(2015), Class WITH 'ariaform', BaseClass WITH 'form', ;
               parent WITH 'ariaformset'
            REPLACE properties WITH FormProp(properties,'ariaform1.'), ;
               Methods WITH FormMethod(Methods,'ariaform1.')
            lcProp = properties
            ChangeText(@lcProp,'ShowWindow =','ShowWindow = 2')
            ChangeText(@lcProp,'TabIndex =','TabIndex = 1')
            REPLACE properties WITH lcProp, objname WITH 'AriaForm1'
            LOCATE ALL FOR LOWER(left(baseclass,10))='formset' AND LOWER(LEFT(class,12))='ariaformset'
            REPLACE properties WITH FormProp(properties,'ariaform1.',.T.), ;
               Methods WITH FormMethod(Methods,'ariaform1.',.T.)
            APPEND FROM tt
          ENDIF

          *WAIT WINDOW 'Compiling Form '+DBF() NOWAIT
          USE
          COMPILE FORM (f+'\'+b[cc,1])
          CLEAR TYPEAHEAD
          KEYBOARD '{CTRL+W}'
          MODIFY FORM (f+'\'+b[cc,1])

        ENDIF

      ENDIF

    NEXT
  ENDIF
RETURN


PROCEDURE CHANGETEXT
PARAMETERS lcText, lcOldVal, lcNewVal
LOCAL lnI, lnC, llFlag
LOCAL ARRAY laLines[1]

lnI=ALINES(lalines,lcText)
llFlag = .F.
lcText = ''
FOR lnC=1 TO lnI
  IF LOWER(laLines[lnC])=LOWER(lcOldVal)
    laLines[lnC] = lcNewVal
    llFlag = .T.
  ENDIF
  lcText = lcText + laLines[lnC]+CHR(13)+CHR(10)
NEXT
IF !llFlag
  lcText = lcText + lcNewVal+CHR(13)+CHR(10)
ENDIF

RETURN

FUNCTION FORMPROP
PARAMETERS lcText, lcSearchVal, llExeclude
LOCAL lnI, lnC, lcV
LOCAL ARRAY laLines[1]

lnI=ALINES(lalines,lcText)
lcV = ''
FOR lnC=1 TO lnI
  DO CASE
    CASE LOWER(laLines[lnC])=LOWER(lcSearchVal) AND !llExeclude
      lcV = lcV + SUBSTR(laLines[lnC],LEN(lcSearchVal)+1)+CHR(13)+CHR(10)
    CASE !LOWER(laLines[lnC])=LOWER(lcSearchVal) AND llExeclude
      lcV = lcV + laLines[lnC]+CHR(13)+CHR(10)
  ENDCASE
NEXT

RETURN lcV

FUNCTION FORMMETHOD
LPARAMETERS lcText, lcSearchVal, llExeclude
LOCAL lnI, lnC, lcV, llFlag
LOCAL ARRAY laLines[1]

lnI=ALINES(lalines,lcText)
llFlag = .F.
lcV = ''

FOR lnC=1 TO lnI

  DO CASE

    CASE LOWER(laLines[lnC])='procedure '+LOWER(lcSearchVal)
      llFlag = .T.
      IF !llExeclude
        lcV = lcV + LEFT(laLines[lnC],10)+SUBSTR(laLines[lnC],10+LEN(lcSearchVal)+1)+CHR(13)+CHR(10)
      ENDIF

    CASE (llFlag AND !llExeclude) OR (!llFlag AND llExeclude)
      lcV = lcV + laLines[lnC]+CHR(13)+CHR(10)
      IF LOWER(laLines[lnC])='endproc'
        llFlag = .F.
      ENDIF

  ENDCASE

NEXT

RETURN lcV

