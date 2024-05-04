using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class ExportData : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.DateTime _exportDate;
    private System.String _notes;
    public ExportData(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public System.DateTime ExportDate
    {
      get
      {
        return _exportDate;
      }
      set
      {
        SetPropertyValue("ExportDate", ref _exportDate, value);
      }
    }
    public System.String Notes
    {
      get
      {
        return _notes;
      }
      set
      {
        SetPropertyValue("Notes", ref _notes, value);
      }
    }

    // Set default image for the entity
    [Action(ToolTip = "Export")]
    public void Export()
    {
        SQLite.SQLiteConnection Sc1 = new SQLite.SQLiteConnection(@"D:\Work\Aria 5 System Admin\1Touch2.DB");
        Sc1.Execute("");
 

    }

  }
}
