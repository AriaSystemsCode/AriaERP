using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class ApplicationKloc
    {

        protected override void OnSaving()
        {
            if (Session.IsNewObject(this))
            {
                if (Application.KlocVersions.Count > 0)
                {
                    foreach (ApplicationKloc version in Application.KlocVersions)
                    {
                        if (version.Oid != Guid.Empty && version.IsActive == true)
                        {
                            if (version.Date < Date)
                            {
                                version.IsActive = false;
                                IsActive = true;
                                version.Save();
                            }
                        }
                    }
                }
            }
            else
            {
                if (Application != null)
                {
                    foreach (ApplicationKloc version in Application.KlocVersions)
                    {
                        if (version.Oid != Guid.Empty && version.IsActive == true && version != this)
                        {
                            if (version.Date < Date)
                            {
                                version.IsActive = false;
                                IsActive = true;
                                version.Save();
                            }
                        }
                    }
                }
                
            }
            base.OnSaving();
        }
    }
}
