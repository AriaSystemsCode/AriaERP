using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module;
namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
  [DefaultClassOptions]
  [IsClient(true, false)]
  // Mahmoud 2/13/2016 Start
  [MapInheritance(MapInheritanceType.ParentTable)]
  [RelatedEntity("Aria5-Windows8Xaml-LocationType")]
    // Mahmoud 2/13/2016 End
  public partial class ClientLocationType : ClientEntity
  {
    public ClientLocationType(Session session)
      : base(session)
    {
    }

    private System.String _Name;
    // Mahmoud 2/13/2016 Start
    [NonPersistentAttribute]
    // Mahmoud 2/13/2016 End
    public System.String Name
    {
        get
        {
            return _Name;
        }
        set
        {
            SetPropertyValue("Name", ref _Name, value);
        }
    }
  }
}
