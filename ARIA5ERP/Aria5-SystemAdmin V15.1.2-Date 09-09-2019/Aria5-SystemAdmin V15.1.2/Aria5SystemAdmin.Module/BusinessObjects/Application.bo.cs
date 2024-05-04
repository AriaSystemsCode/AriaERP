using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Application_T
    {
        protected override void OnSaving()
        {
            if (KlocVersions.Count > 0)
            {
                ApplicationKloc latestversion = KlocVersions[0];
                for (int i = 1; i < KlocVersions.Count; i++)
                {
                    if (latestversion.Date < KlocVersions[i].Date)
                    {
                        latestversion.IsActive = false;
                        latestversion.Save();
                        latestversion = KlocVersions[i];
                        
                    }
                }
                latestversion.IsActive = true;
                latestversion.Save();
            }
            base.OnSaving();
        }
    }
}
