// Developer Express Code Central Example:
// How to: Add a Button to a Form
// 
// This example illustrates how to add button to a form. The complete description
// is available in the How to: Add a Button to a Form
// (ms-help://DevExpress.Xaf/CustomDocument2816.htm) help topic.
// 
// You can find sample updates and versions for different programming languages here:
// http://www.devexpress.com/example=E1847

using System;
using System.Collections.Generic;
using System.Text;
using DevExpress.ExpressApp.Editors;
using System.Web.UI.WebControls;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Web.Editors.ASPx;
using DevExpress.Web;
using DevExpress.ExpressApp.Model;
using DevExpress.ExpressApp.Layout;
namespace AriaDevExpress.Module.Web.Controllers.WebSite
{
    public interface IModelButtonDetailViewItemWeb : IModelViewItem { }
    [ViewItemAttribute(typeof(IModelButtonDetailViewItemWeb))]
    public class ButtonDetailViewItemWeb : ViewItem
    {
        public ButtonDetailViewItemWeb(IModelViewItem model, Type objectType)
            : base(objectType, model.Id)
        {
            CreateControl();
        }
        protected override object CreateControlCore()
        {
            ASPxButton button = new ASPxButton();
            button.Text = "MyButton Two";
            button.EnableClientSideAPI = true;
            button.Click += new EventHandler(button_Click);
            string strconfirm = "function show_alert() {var msg = 'Button Two Click';alert(msg);}";
            button.ClientSideEvents.Click = strconfirm;
            return button;
        }
        void button_Click(object sender, EventArgs e)
        {
            // Perform the required actions on the server side.
        }
    }
}
