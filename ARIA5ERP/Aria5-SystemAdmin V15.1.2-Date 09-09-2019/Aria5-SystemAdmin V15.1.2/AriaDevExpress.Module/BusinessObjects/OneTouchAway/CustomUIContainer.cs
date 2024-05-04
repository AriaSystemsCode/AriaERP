using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AriaDevExpress.Module.BusinessObjects.OneTouchAway
{
    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "E-mail Template")]
    public class OTAWAYEmailTemplateClass { }

    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "E-mail Setting")]
    public class OTAWAYEmailSettingClass { }

    [DevExpress.Xpo.NonPersistent]
    public class OTAWAYSortCategory { }

    [DevExpress.Xpo.Custom("Caption", "Sort App\\Modules")]
    [DevExpress.Xpo.NonPersistent]
    public class OTAWAYSortApp { }

    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "Sort Applications")]
    public class OTAWAYMR_Application { }
}
