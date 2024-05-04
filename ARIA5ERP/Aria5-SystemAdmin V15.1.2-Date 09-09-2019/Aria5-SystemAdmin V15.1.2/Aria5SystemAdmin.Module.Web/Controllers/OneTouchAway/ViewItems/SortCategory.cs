using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.OneTouchAway
{
    public interface IOTAWAYSortCategoryViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(IOTAWAYSortCategoryViewItem))]
    public class OTAWAYSortCategoryViewItem : ViewItem
    {
        public OTAWAYSortCategoryViewItem(Type objectType, string id) : base(objectType, id) { }
        public OTAWAYSortCategoryViewItem(IOTAWAYSortCategoryViewItem model, Type objectType) : base(objectType, model.Id) { }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/OneTouchAway/SortCategory.ascx");
        }
    }
}