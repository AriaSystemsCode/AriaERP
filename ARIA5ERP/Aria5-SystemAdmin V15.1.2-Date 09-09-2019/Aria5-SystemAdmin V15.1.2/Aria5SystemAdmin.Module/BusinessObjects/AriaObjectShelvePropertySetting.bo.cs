using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelvePropertySetting
    {
        protected override void OnChanged(string propertyName, object oldValue, object newValue)
        {
            base.OnChanged(propertyName, oldValue, newValue);

            if (!IsLoading && Oid != Guid.Empty)
            {
                Modified = true;
            }
        }
    }
}
