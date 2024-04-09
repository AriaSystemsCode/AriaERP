using System;
using System.Collections.Generic;
using System.Linq;
using System.Resources;
using System.Text;
using System.Threading.Tasks;

namespace Aria.PaymentGateway
{
    public static class AriaResourcesHelper
    {
        public static string GetString(this ResourceManager resourceManager, string key, string defaultValue)
        {
            return resourceManager.GetString(key) == null ? defaultValue : resourceManager.GetString(key);
        }
    }
}
