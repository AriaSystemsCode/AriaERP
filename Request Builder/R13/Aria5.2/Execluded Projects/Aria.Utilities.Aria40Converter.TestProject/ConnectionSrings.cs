using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria.Utilities.Aria40Converter.TestProject
{
    public class ConnectionSrings
    {
        public static string Aria27 = @"Driver={Microsoft Visual FoxPro Driver};sourcedb=C:\ARIA\ARIA27\SYSFILES;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";
        public static string AriaXp = @"Driver={Microsoft Visual FoxPro Driver};sourcedb=C:\ARIA\ARIA4XP\SQLDictionary;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";
        public static string Aria27Merage =@"E:\RequestBuilder\Aria.Utilities.Aria40Converter\bin\Debug\MergeData\Aria27\";
        public static string AriaXpMerage = @"E:\RequestBuilder\Aria.Utilities.Aria40Converter\bin\Debug\MergeData\Aria4XP\";
    }
}
