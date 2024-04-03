
SET CLASSLIB TO "C:\My Work\Tasks\OsamaHussienCombine\ServerClasses\ariaaldbcentric.vcx" additive

PUBLIC objProgram
objProgram = CREATEOBJECT("ariaaldbcentric.alautal")

PUBLIC objFilter
objFilter = CREATEOBJECT("Aria.DataTypes.AriaOptionGridXmlDataSet")
objFilter.FileName = "C:\My Work\Tasks\OsamaHussienCombine\OUTPUT\" + "OptionGridFilter.xml"

PUBLIC objAllocate
objAllocate = CREATEOBJECT("Aria.DataTypes.AriaOptionGridXmlDataSet")
objAllocate.FileName = "C:\My Work\Tasks\OsamaHussienCombine\OUTPUT\" + "OptionGridAllocate.xml"

PUBLIC objPickTicket		
objPickTicket = CREATEOBJECT("Aria.DataTypes.AriaOptionGridXmlDataSet")
objPickTicket.FileName = "C:\My Work\Tasks\OsamaHussienCombine\OUTPUT\" + "OptionGridPickTicket.xml"

objProgram.Run("99",  objFilter, objAllocate, objPickTicket)