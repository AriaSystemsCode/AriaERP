using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using AriaDevExpress.Module.DataContext;
using AriaDevExpress.Module.OneTouchAwayDataContext;

namespace AriaDevExpress.Web.UserControls.OneTouchAway
{
    public partial class SortApplicationModule : System.Web.UI.UserControl
    {

        OTAWAYAriaOnlineDataContext db = new OTAWAYAriaOnlineDataContext();

          
        protected void AllModuleDataSource_Selecting(object sender, LinqDataSourceSelectEventArgs e)
        {
            if (e.WhereParameters["ParentID"] == null)
                e.Cancel = true;
        }

        protected void btnSave_Click(object sender, EventArgs e)
        {
            OTAWAYAriaOnlineDataContext db = new OTAWAYAriaOnlineDataContext();
            foreach (DevExpress.Web.ListEditItem item in lstSelected.Items)
            {
                var appModIlist = db.OTAWAYApplicationModules.Where(i => i.AppModID == Convert.ToInt32(item.Value));
                if (appModIlist.Count() > 0)
                {
                    AriaDevExpress.Module.OneTouchAwayDataContext.OTAWAYApplicationModule appModItem = appModIlist.First();
                    appModItem.AppRank = item.Index;
                }
            }
            db.SubmitChanges();
        }
    }
}