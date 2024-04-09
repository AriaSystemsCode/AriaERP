using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.HelperClass.SAAS
{
    public class AriaApplicationChecker
    {
        /// <summary>
        /// To Check if COM+ Application Created or not.
        /// </summary>
        /// <example>
        /// <code>
        /// AriaEnviromentVariables Env = new AriaEnviromentVariables();
        /// Env.IsApplicationExists("Aria4XP");
        /// </code>
        /// </example>
        /// <param name="applicationName"></param>
        /// <returns></returns>
        public static bool IsApplicationExists(string applicationName)
        {
            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();
            COMAdmin.COMAdminCatalogCollection applications = (COMAdmin.COMAdminCatalogCollection)catalog.GetCollection("Applications");
            applications.Populate();
            COMAdmin.COMAdminCatalogObject application = null;
            for (int index = 0; index < applications.Count; index++)
            {
                application = (COMAdmin.COMAdminCatalogObject)applications.get_Item(index);

                if (applicationName == application.Name.ToString())
                    return true;
            }

            return false;
        }
    }
}
