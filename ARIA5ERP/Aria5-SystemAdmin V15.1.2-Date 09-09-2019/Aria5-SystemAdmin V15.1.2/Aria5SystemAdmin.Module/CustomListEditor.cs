//using System;
//using System.Collections.Generic;
//using System.Text;
//using DevExpress.Persistent.Base;
//using DevExpress.Persistent.BaseImpl;
////using System.Drawing;
//using DevExpress.Xpo;
//using DevExpress.Xpo.Metadata;
////using DevExpress.ExpressApp.Demos;
//using System.Collections;
//using System.Web.UI.WebControls;
//using DevExpress.ExpressApp;
//using DevExpress.ExpressApp.Editors;
//using System.Globalization;
//using DevExpress.Web;
//using DevExpress.ExpressApp.Web;
//using DevExpress.ExpressApp.Web.Editors.ASPx;
//using DevExpress.ExpressApp.Model;
//using Aria5SystemAdmin.Module.BusinessObjects;
//using DevExpress.Utils;
//using DevExpress.ExpressApp.Web.Editors;

//namespace DevExpress.Web.ASPxEditors
//{

//    public interface IPictureItem
//    {
//        string ID { get; }
//        Image Image { get; }
//        string Text { get; }
//    }

//    //[Hint(Hints.CustomListEditorObjectHint)]
//    //[System.ComponentModel.DisplayName(Captions.ListEditors_CustomListEditor)]
//    //[ImageName("ListEditors.Demo_ListEditors_Custom")]
//    //public class CustomListEditorDemoObject : BaseObject, IPictureItem
//    //{
//    //    public CustomListEditorDemoObject(Session session) : base(session) { }
//    //    [DisplayName]
//    //    public string Title
//    //    {
//    //        get { return GetPropertyValue<string>("Title"); }
//    //        set { SetPropertyValue<string>("Title", value); }
//    //    }
//    //    [Size(SizeAttribute.Unlimited), ValueConverter(typeof(ImageValueConverter))]
//    //    public Image Cover
//    //    {
//    //        get { return GetPropertyValue<Image>("Cover"); }
//    //        set { SetPropertyValue<Image>("Cover", value); }
//    //    }
//    //    public String Director
//    //    {
//    //        get { return GetPropertyValue<String>("Director"); }
//    //        set { SetPropertyValue<String>("Director", value); }
//    //    }
//    //    #region IPictureItem Members
//    //    Image IPictureItem.Image
//    //    {
//    //        get { return Cover; }
//    //    }
//    //    string IPictureItem.Text
//    //    {
//    //        get { return String.Format("{0} by {1}", Title, Director); }
//    //    }
//    //    string IPictureItem.ID
//    //    {
//    //        get { return Oid.ToString(); }
//    //    }
//    //    #endregion
//    //}


   

//    [ListEditor(typeof(IPictureItem))]
//    public class ASPxCustomListEditor : ListEditor
//    {
//        private ASPxObjectContainer control;
//        private ASPxTreeView control1;
//        private ASPxListBox  control2;
//        private ComplexWebListEditor  control3;

//        private Object dataSource;
//        private IModelListView info1;
//        public ASPxCustomListEditor(IModelListView info) : base(info) {

//            this.info1 = info;
//        }
        
//        protected override object CreateControlsCore()
//        {
//            control = new ASPxObjectContainer();

//            control1 = new ASPxTreeView();
             
//            //control2 = new ASPxListBox();
//            //control3 = new ASPxGridListEditor(this.info1);
                
//            control.Controls.Add(control1);
//            //control.Controls.Add(control2);

//            control.ID = "CustomListEditor_control";
//            return control;
//        }
//        protected override void AssignDataSourceToControl(Object dataSource)
//        {
//            this.dataSource = dataSource;

//            if (control != null)
//            {
//                //control.DataSource = dataSource;
//                //control2.DataSource = dataSource;
//                //String[] myList = new string[4];

//                //myList[0] = "One";
//                //myList[1] = "Two";
//                //myList[2] = "Three";
//                //myList[3] = "Four";

//                //control2.Items.AddRange(myList);
//               //control2.DataSource = dataSource;
//               // control2.DataBind(); 

//                //control3.DataSource = dataSource;
//                //control3.DataBind();
//                //control3.Visible = true;

  

//                //((Setup)((XPCollection)((ProxyCollection)dataSource).OriginalCollection)[1]).Application.Name
//                int counter = ((ProxyCollection)dataSource).Count;
//                string RootNode = "";
//                RootNode = ((Setup)((XPCollection)((ProxyCollection)dataSource).OriginalCollection)[0]).Application.Name;

//                ((ASPxTreeView)control1).Nodes.Add(RootNode, RootNode);
//                string EntiryNodeString = "";
//                string CompanyNodeString = "";

//                for (int i = 0; i < counter; i++)
//                {

//                    string EntiryNode = "";
//                    string CompanyNode = "";
//                    try
//                    {
//                        EntiryNode = ((Setup)((XPCollection)((ProxyCollection)dataSource).OriginalCollection)[i]).Id.ObjectName.ToString();

//                    }
//                    catch (Exception ex)
//                    { }
//                    try
//                    {
//                        CompanyNode = ((Setup)((XPCollection)((ProxyCollection)dataSource).OriginalCollection)[i]).Company.Name.ToString();

//                    }
//                    catch (Exception ex)
//                    { }
//                    //NodeString = ((Setup)((XPCollection)((ProxyCollection)dataSource).OriginalCollection)[i]).SetupName.ToString();
//                    if (string.IsNullOrEmpty(CompanyNode) == false && CompanyNodeString.IndexOf(CompanyNode) < 0)
//                    { //((ASPxTreeView)control1).Nodes.Add(CompanyNode);
//                        CompanyNodeString = CompanyNode + "|" + CompanyNode;
//                    }
//                    if (string.IsNullOrEmpty(EntiryNode) == false && EntiryNodeString.IndexOf(EntiryNode) < 0)
//                    { //((ASPxTreeView)control1).Nodes.Add(EntiryNode);
//                        EntiryNodeString = EntiryNodeString + "|" + EntiryNode;
//                    }

//                }
//                String[] EntiryNodeStringArr = EntiryNodeString.Split('|');

//                for (int i = 1; i < EntiryNodeStringArr.Length; i++)
//                {
//                    ((ASPxTreeView)control1).Nodes[0].Nodes.Add(EntiryNodeStringArr[i],EntiryNodeStringArr[i]);
//                }

//                String[] CompanyNodeStringArr = CompanyNodeString.Split('|');

//                for (int i = 1; i < CompanyNodeStringArr.Length; i++)
//                {
//                    ((ASPxTreeView)control1).Nodes[0].Nodes.Add(CompanyNodeStringArr[i],CompanyNodeStringArr[i]);
//                    int xnode = ((ASPxTreeView)control1).Nodes.FindByName(CompanyNodeStringArr[i]).Index;

//                    for (int j = 1; j < EntiryNodeStringArr.Length; j++)
//                    {
//                        ((ASPxTreeView)control1).Nodes[0].Nodes[xnode].Nodes.Add(EntiryNodeStringArr[j], EntiryNodeStringArr[j]);
//                    }
//                }

//            }
//        }




//        public override void Refresh()
//        {
//            if (control != null) control1.RefreshVirtualTree();
//        }
//        public override bool AllowEdit
//        {
//            get
//            {
//                return false;
//            }
//            set
//            {
//            }
//        }

//        public override SelectionType SelectionType
//        {
//            get { return SelectionType.TemporarySelection; }
//        }
//        public override IList GetSelectedObjects()
//        {
//            List<object> selectedObjects = new List<object>();
//            if (FocusedObject != null)
//            {
//                selectedObjects.Add(FocusedObject);
//            }
//            return selectedObjects;
//        }
//        public override DevExpress.ExpressApp.Templates.IContextMenuTemplate
//           ContextMenuTemplate
//        {
//            get { return null; }
//        }
//        public override void SaveModel() { }


//       // protected override object CreateControlsCore()
//        //{
//           // control1.NodeClick+= new EventHandler(control_OnClick);
//             //control.OnClick += new EventHandler<ThumbnailClickEventArgs>(control_OnClick);
//     //
//         //   base.CreateControls();
//      //  }
//        private void control_OnClick(object sender, EventArgs e)
//        {
//            //this.FocusedObject = e.ItemClicked;
//           // OnSelectionChanged();
//            //OnProcessSelectedItem();
//        }
//        private object focusedObject;
//        public override object FocusedObject
//        {
//            get
//            {
//                return focusedObject;
//            }
//            set
//            {
//                focusedObject = value;
//            }
//        }
//    }

//}
