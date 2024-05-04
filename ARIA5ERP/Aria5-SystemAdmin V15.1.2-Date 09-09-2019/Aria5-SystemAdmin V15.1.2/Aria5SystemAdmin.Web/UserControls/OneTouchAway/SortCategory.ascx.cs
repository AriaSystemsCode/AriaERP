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
    public partial class Sort : System.Web.UI.UserControl
    {
        protected void NonSelectedLinqDataSource_Selecting(object sender, LinqDataSourceSelectEventArgs e)
        {
            if (e.WhereParameters["Category_ID"] == null)
                e.Cancel = true;
        }
        protected void btnSave_Click(object sender, EventArgs e)
        {
            int categoryID = Convert.ToInt32(CategoryDropDownList.SelectedItem.Value);
            OTAWAYAriaOnlineDataContext db = new OTAWAYAriaOnlineDataContext();
            foreach (DevExpress.Web.ListEditItem item in lstSelected.Items)
            {
                var article = db.OTAWAYArticles.FirstOrDefault(i => i.Article_ID == Convert.ToInt32(item.Value));
                if (article != null)
                {
                    article.VisibleRank =Convert.ToInt16(item.Index);
                    article.Category_ID = categoryID;
                }
            }

            foreach (DevExpress.Web.ListEditItem item in lstNonSelected.Items)
            {
                var article = db.OTAWAYArticles.FirstOrDefault(i => i.Article_ID == Convert.ToInt32(item.Value));
                if (article != null)
                {
                    article.Category_ID = null;
                }
            }
            db.SubmitChanges();
        }

    }

}


