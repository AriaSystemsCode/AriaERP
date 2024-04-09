*--- Fix BOMLINE for Cathy Daiels by Adding two dummy records for operations
*--- CU and SE
*--- By SSH
*--- Due to B#604893
SELECT CUTTKTH
SET ORDER TO 1
SELECT CUTTKTL
SET ORDER TO 1
SET RELATION TO Cuttktl.cuttkt INTO Cuttkth ADDITIVE
GO TOP
SCAN FOR CUTTKTH.STATUS = "O"
  WAIT WINDOW "Fix CutTkt#: " +CutTkt NOWAIT
  SELECT BOMLINE
  *--- cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
  *--- "M" + "1" +CutTktNo+STR(LineNo,6)
  APPEND BLANK
  REPLACE CimTyp   WITH "M" ,;
          mfgCode  WITH "CU",;
          CtktNo   WITH CutTktL.CutTkt,;
          LineNo   WITH CutTktL.LineNo,;
          Style    WITH CutTktL.Style,;
          UnitQty  WITH 1,;
          StyQty   WITH CutTktl.TotQty,;
          ItemQty  WITH CutTktl.TotQty,;
          cBomTyp  WITH "3",;
          cType    WITH "1",;
          cCatgTyp WITH "M",;
          cSizes   WITH "12345678"

  APPEND BLANK
  REPLACE CimTyp   WITH "M" ,;
          mfgCode  WITH "SE",;
          CtktNo   WITH CutTktL.CutTkt,;
          LineNo   WITH CutTktL.LineNo,;
          Style    WITH CutTktL.Style,;
          UnitQty  WITH 1,;
          StyQty   WITH CutTktl.TotQty,;
          ItemQty  WITH CutTktl.TotQty,;
          cBomTyp  WITH "3",;
          cType    WITH "1",;
          cCatgTyp WITH "M",;
          cSizes   WITH "12345678"

ENDSCAN