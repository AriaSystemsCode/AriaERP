﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using AriaDevExpress.Module.DataContext;

namespace AriaDevExpress.Web.UserControls.WebSite
{
    public partial class SortApplicationModule : System.Web.UI.UserControl
    {
        AriaOnlineDataContext db = new AriaOnlineDataContext();

        protected void AllModuleDataSource_Selecting(object sender, LinqDataSourceSelectEventArgs e)
        {
            if (e.WhereParameters["ParentID"] == null)
                e.Cancel = true;
        }

        protected void btnSave_Click(object sender, EventArgs e)
        {
            AriaDevExpress.Module.DataContext.AriaOnlineDataContext db = new AriaDevExpress.Module.DataContext.AriaOnlineDataContext();
            foreach (DevExpress.Web.ListEditItem item in lstSelected.Items)
            {
                var appModIlist = db.ApplicationModules.Where(i => i.AppModID == Convert.ToInt32(item.Value));
                if (appModIlist.Count() > 0)
                {
                    AriaDevExpress.Module.DataContext.ApplicationModule appModItem = appModIlist.First();
                    appModItem.AppRank = item.Index;
                }
            }
            db.SubmitChanges();
        }
    }
}