 LPARAMETERS tcclass, tclibrary, txparm1, txparm2, txparm3, txparm4, txparm5, txparm6, txparm7, txparm8, txparm9, txparm10, txparm11, txparm12, txparm13, txparm14, txparm15, txparm16, txparm17
 LOCAL lclibrary, loobject
 DO CASE
    CASE TYPE('tcLibrary')<>'C' .OR. EMPTY(tclibrary) .OR. UPPER(tclibrary)$UPPER(SET('CLASSLIB')) .OR. UPPER(tclibrary)$UPPER(SET('PROCEDURE')) .OR. UPPER(tclibrary)$UPPER(SET('LIBRARY'))
       lclibrary = ''
    CASE FILE(tclibrary)
       lclibrary = UPPER(tclibrary)
    OTHERWISE
       lclibrary = UPPER(LOCFILE(tclibrary, 'Visual Class Library:VCX;Program:PRG', tclibrary))
 ENDCASE
 DO CASE
    CASE EMPTY(lclibrary)
    CASE RIGHT(lclibrary, 4)='.VCX'
       SET CLASSLIB TO (lclibrary) ADDITIVE
    OTHERWISE
       SET PROCEDURE TO (lclibrary) ADDITIVE
 ENDCASE
 DO CASE
    CASE TYPE("txParm1")="C" .AND. (("askfilters"$txparm1) .OR. ("fltval"$txparm1))
       loobject = CREATEOBJECT(tcclass, @txparm1, @txparm2, @txparm3, @txparm4, @txparm5, @txparm6, @txparm7, @txparm8, @txparm9, @txparm10, @txparm11, @txparm12, @txparm13, @txparm14, @txparm15, @txparm16, @txparm17)
    CASE PCOUNT()<3
       loobject = CREATEOBJECT(tcclass)
    CASE PCOUNT()=3
       loobject = CREATEOBJECT(tcclass, @txparm1)
    CASE PCOUNT()=4
       loobject = CREATEOBJECT(tcclass, @txparm1, @txparm2)
    CASE PCOUNT()=5
       loobject = CREATEOBJECT(tcclass, @txparm1, @txparm2, @txparm3)
    CASE PCOUNT()=6
       loobject = CREATEOBJECT(tcclass, @txparm1, @txparm2, @txparm3, @txparm4)
 ENDCASE
 DO CASE
    CASE EMPTY(lclibrary) .OR.  .NOT. (lclibrary$UPPER(SET('CLASSLIB')) .OR. lclibrary$UPPER(SET('PROCEDURE')) .OR. lclibrary$UPPER(SET('LIBRARY')))
    CASE RIGHT(lclibrary, 4)='.VCX'
       RELEASE CLASSLIB (lclibrary)
    OTHERWISE
       RELEASE PROCEDURE (lclibrary)
 ENDCASE
 RETURN loobject
ENDFUNC
**
