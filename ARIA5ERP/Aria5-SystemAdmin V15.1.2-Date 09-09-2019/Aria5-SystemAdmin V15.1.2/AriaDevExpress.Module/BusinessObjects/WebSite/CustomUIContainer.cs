using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "E-mail Template")]
    public class EmailTemplateClass { }

    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "E-mail Setting")]
    public class EmailSettingClass { }

    [DevExpress.Xpo.NonPersistent]
    public class SortCategory { }

    [DevExpress.Xpo.Custom("Caption", "Sort App\\Modules")]
    [DevExpress.Xpo.NonPersistent]
    public class SortApp { }

    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "Sort Applications")]
    public class MR_Application { }
}
