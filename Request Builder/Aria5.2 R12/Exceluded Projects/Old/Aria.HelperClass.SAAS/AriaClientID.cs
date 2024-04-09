/**********************************************************************************************
*: System: ARIA APPAREL SYSTEM
*:*********************************************************************************************
*: Developer: Ahmed Maher Keshk (AMK)
*:*********************************************************************************************
*: Date  : 10/03/2009
*:*********************************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;

namespace Aria.HelperClass.SAAS
{
    /// <summary>
    /// Used to get Client Id from Aria Activation Key file.
    /// </summary>
    public class AriaClientID
    {
        /// <summary>
        /// Used to get client id depend on Aria Activation Key file path.
        /// </summary>
        /// <example>
        /// <code>
        /// AriaClientID ariaClientId= new AriaClientID();
        /// ariaClientId.GetClientId("C:\\Aria27\\SYSFILES\\act_key.bin")
        /// </code>
        /// </example>
        /// <param name="paths">Aria Activation Key file path</param>
        /// <returns>Client Id As string Datatype</returns>
        public string Get_ClientId(string paths)
        {
            try
            {
                StreamReader s = new StreamReader(MapDrive.GetUNCPath(paths));
                s.BaseStream.Position = 12876;
                string Act_key = s.ReadToEnd().Trim();
                return Act_key.Substring(0, 5);
            }
            catch (Exception Ex)
            {
                EventLog.CreateEventSource("Aria.HelperClass.SAAS", "Aria Configuration Server");
                EventLog.WriteEntry("Aria.HelperClass.SAAS", "AriaClientID.GetClientId(paths): " + Ex.Message,
                    EventLogEntryType.Error);
                return "";
            }
        }
    }
}
