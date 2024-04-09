PARAMETERS lcMfgOrdNo,llAutoGen
DO (gcAppHome+'MFCSSH') WITH 'T',IIF(TYPE('lcMfgOrdNo')='C',lcMfgOrdNo,''),llAutoGen