PUBLIC objProgram
SET CLASSLIB TO "C:\My Work\Tasks\OsamaHussienCombine\ServerClasses\ariaaldbcentric.vcx" additive

objProgram = CREATEOBJECT("ariaaldbcentric.alautp")

PUBLIC objPackingOption
objPackingOption = CREATEOBJECT("Aria.DataTypes.AriaOptionGridXmlDataSet")
objPackingOption.FileName = "C:\My Work\Tasks\OsamaHussienCombine\OUTPUT\OptionGridPacking.xml"

objProgram.Run("99",  objPackingOption)