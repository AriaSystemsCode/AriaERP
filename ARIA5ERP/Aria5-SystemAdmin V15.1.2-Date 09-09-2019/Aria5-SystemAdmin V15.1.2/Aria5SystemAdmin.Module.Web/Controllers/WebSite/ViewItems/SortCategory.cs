using System;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;

namespace AriaDevExpress.Module.Web.Controllers.WebSite
{
    public interface ISortCategoryViewItem : IModelViewItem { }
    [ViewItemAttribute(typeof(ISortCategoryViewItem))]
    public class SortCategoryViewItem : ViewItem
    {
        public SortCategoryViewItem(Type objectType, string id) : base(objectType, id) { }
        public SortCategoryViewItem(ISortCategoryViewItem model, Type objectType) : base(objectType, model.Id) { }
        protected override object CreateControlCore()
        {
            return WebWindow.CurrentRequestPage.LoadControl("UserControls/WebSite/SortCategory.ascx");
        }
    }
}
