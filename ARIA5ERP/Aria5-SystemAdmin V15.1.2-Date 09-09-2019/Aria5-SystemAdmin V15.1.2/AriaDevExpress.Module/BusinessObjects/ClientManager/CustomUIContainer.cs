using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.ClientManager
{


    [DevExpress.Xpo.Custom("Caption", "Sara")]
    [DevExpress.Xpo.NonPersistent]
    [DefaultClassOptions]
    public class Sara
    {
    }

    [DevExpress.Xpo.Custom("Caption", "New Client1")]
    [DevExpress.Xpo.NonPersistent]
    public class NewClient1
    {
    }

    [DevExpress.Xpo.Custom("Caption", "Configration")]
    [DevExpress.Xpo.NonPersistent]
    public class Configration
    {
    }
}
