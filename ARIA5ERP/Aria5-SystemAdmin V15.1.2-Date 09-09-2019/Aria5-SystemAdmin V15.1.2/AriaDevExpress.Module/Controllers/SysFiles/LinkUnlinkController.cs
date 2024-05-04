using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Web.SystemModule;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using DevExpress.ExpressApp.SystemModule;

namespace AriaDevExpress.Module.Controllers.SysFiles
{
    public partial class LinkUnlinkController : ViewController
    {
        public LinkUnlinkController()
        {
            InitializeComponent();
            RegisterActions(components);
        }

        protected override void OnViewChanged()
        {
            RemoveDeleteButton();
            base.OnViewChanged();
        }

        protected override void EndUpdate()
        {
            DisableLinkUnlink();
            base.EndUpdate();
        }
        void DisableLinkUnlink()
        {
            WebLinkUnlinkController LinkUnlinkController = Frame.GetController<WebLinkUnlinkController>();
            DeleteObjectsViewController deleteController = Frame.GetController<DeleteObjectsViewController>();

            if (this.View != null &&
                this.View is ListView &&
                this.Frame != null &&
                this.Frame is DevExpress.ExpressApp.NestedFrame &&
                ((NestedFrame)this.Frame).ViewItem.CurrentObject != null &&
                ((NestedFrame)this.Frame).ViewItem.CurrentObject is IAuditable
                && (LinkUnlinkController.LinkAction.Active.ResultValue || deleteController.DeleteAction.Active.ResultValue))
            {
                if (((XPBaseObject)((NestedFrame)this.Frame).ViewItem.CurrentObject).ClassInfo.FindMember("Ticket") != null)
                {
                    string tkt = ((string)((XPBaseObject)((NestedFrame)this.Frame).ViewItem.CurrentObject).GetMemberValue("Ticket"));
                    if (string.IsNullOrWhiteSpace(tkt))
                    {

                        LinkUnlinkController.LinkAction.Enabled.SetItemValue("TicketNotFound", false);
                        LinkUnlinkController.LinkAction.ToolTip = "Please Enter Ticket before linking/unlinking Objects!";

                        LinkUnlinkController.UnlinkAction.Enabled.SetItemValue("TicketNotFound", false);
                        LinkUnlinkController.UnlinkAction.ToolTip = "Please Enter Ticket before linking/unlinking Objects!";

                        deleteController.DeleteAction.Enabled.SetItemValue("TicketNotFound", false);
                        deleteController.DeleteAction.ToolTip = "Please Enter Ticket before Deleting Objects!";

                        return;
                    }
                }
            }
            EnableLinkUnlink();
        }
        void EnableLinkUnlink()
        {
            WebLinkUnlinkController LinkUnlinkController = Frame.GetController<WebLinkUnlinkController>();
            DeleteObjectsViewController deleteController = Frame.GetController<DeleteObjectsViewController>();

            LinkUnlinkController.LinkAction.Enabled.SetItemValue("TicketNotFound", true);
            LinkUnlinkController.LinkAction.ToolTip = LinkUnlinkController.LinkAction.ToolTip.Replace("Please Enter Ticket before linking/unlinking Objects!", "");

            LinkUnlinkController.UnlinkAction.Enabled.SetItemValue("TicketNotFound", true);
            LinkUnlinkController.UnlinkAction.ToolTip = LinkUnlinkController.UnlinkAction.ToolTip.Replace("Please Enter Ticket before linking/unlinking Objects!", "");

            deleteController.DeleteAction.Enabled.SetItemValue("TicketNotFound", true);
            deleteController.DeleteAction.ToolTip = deleteController.DeleteAction.ToolTip.Replace("Please Enter Ticket before Deleting Objects!", "");
        }

        void RemoveDeleteButton()
        {
            WebLinkUnlinkController LinkUnlinkController = Frame.GetController<WebLinkUnlinkController>();

            var deleteController = Frame.GetController<DeleteObjectsViewController>();
            deleteController.DeleteAction.Active.SetItemValue("RemoveDelete", !LinkUnlinkController.LinkAction.Active.ResultValue);
        }
    }
}
