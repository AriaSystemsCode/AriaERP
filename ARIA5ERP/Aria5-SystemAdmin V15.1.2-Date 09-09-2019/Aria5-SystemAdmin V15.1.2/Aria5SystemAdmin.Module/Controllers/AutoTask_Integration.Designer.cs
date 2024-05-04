namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AutoTask_Integration
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.creatproject = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.updateproject = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.download_AHt_file = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.calculate_dot = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CreateTrackingtaskonautotask = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.Calculate_AHT = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.ShowTaskNotes = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.DOT_download = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.Get_DOT_data = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CreateWBSPhase = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.GetResource = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.Calculate_AHT_for_ticket = new DevExpress.ExpressApp.Actions.ParametrizedAction(this.components);
            this.AHTForCompletedTickets = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.AHTForNotCompletedTickets = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // creatproject
            // 
            this.creatproject.Caption = "Create Project";
            this.creatproject.ConfirmationMessage = null;
            this.creatproject.Id = "creat_project";
            this.creatproject.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.creatproject.ToolTip = null;
            this.creatproject.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.creatproject.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.creatproject_Execute);
            // 
            // updateproject
            // 
            this.updateproject.Caption = "Update Project";
            this.updateproject.ConfirmationMessage = null;
            this.updateproject.Id = "update_project";
            this.updateproject.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.updateproject.ToolTip = null;
            this.updateproject.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.updateproject.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.updateproject_Execute);
            // 
            // download_AHt_file
            // 
            this.download_AHt_file.Caption = "Download The Report";
            this.download_AHt_file.ConfirmationMessage = null;
            this.download_AHt_file.Id = "downloadAHtfile";
            this.download_AHt_file.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AverageHandleTime);
            this.download_AHt_file.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.download_AHt_file.ToolTip = null;
            this.download_AHt_file.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.download_AHt_file.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.download_AHt_file_Execute);
            // 
            // calculate_dot
            // 
            this.calculate_dot.Caption = "Calculate OTD";
            this.calculate_dot.ConfirmationMessage = null;
            this.calculate_dot.Id = "calculate dot";
            this.calculate_dot.ToolTip = null;
            this.calculate_dot.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.calculate_dot_Execute);
            // 
            // CreateTrackingtaskonautotask
            // 
            this.CreateTrackingtaskonautotask.Caption = "Craete Task";
            this.CreateTrackingtaskonautotask.ConfirmationMessage = null;
            this.CreateTrackingtaskonautotask.Id = "CreateTrackingtaskonautotask";
            this.CreateTrackingtaskonautotask.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingTask);
            this.CreateTrackingtaskonautotask.ToolTip = null;
            this.CreateTrackingtaskonautotask.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CreateTrackingtaskonautotask_Execute);
            // 
            // Calculate_AHT
            // 
            this.Calculate_AHT.Caption = "Calculate AHT";
            this.Calculate_AHT.ConfirmationMessage = null;
            this.Calculate_AHT.Id = "CalculateAHT";
            this.Calculate_AHT.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AverageHandleTime);
            this.Calculate_AHT.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.Calculate_AHT.ToolTip = null;
            this.Calculate_AHT.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.Calculate_AHT.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.Calculate_AHT_Execute);
            // 
            // ShowTaskNotes
            // 
            this.ShowTaskNotes.AcceptButtonCaption = null;
            this.ShowTaskNotes.CancelButtonCaption = null;
            this.ShowTaskNotes.Caption = "Show Task Notes";
            this.ShowTaskNotes.Category = "Edit";
            this.ShowTaskNotes.ConfirmationMessage = null;
            this.ShowTaskNotes.Id = "ShowTaskNotes";
            this.ShowTaskNotes.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingTask);
            this.ShowTaskNotes.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ShowTaskNotes.ToolTip = null;
            this.ShowTaskNotes.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ShowTaskNotes.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.ShowTaskNotes_CustomizePopupWindowParams);
            this.ShowTaskNotes.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.ShowTaskNotes_Execute);
            // 
            // DOT_download
            // 
            this.DOT_download.Caption = "Download OTD Report";
            this.DOT_download.ConfirmationMessage = null;
            this.DOT_download.Id = "DOT_download";
            this.DOT_download.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.DeliveryOnTime);
            this.DOT_download.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.DOT_download.ToolTip = null;
            this.DOT_download.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.DOT_download.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.DOT_download_Execute);
            // 
            // Get_DOT_data
            // 
            this.Get_DOT_data.Caption = "get_dot_row_data";
            this.Get_DOT_data.ConfirmationMessage = null;
            this.Get_DOT_data.Id = "get_dot_row_data";
            this.Get_DOT_data.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.DeliveryOnTime);
            this.Get_DOT_data.ToolTip = null;
            this.Get_DOT_data.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.Get_DOT_data_Execute);
            // 
            // CreateWBSPhase
            // 
            this.CreateWBSPhase.Caption = "Create Phase";
            this.CreateWBSPhase.ConfirmationMessage = null;
            this.CreateWBSPhase.Id = "CreateWBSPhase";
            this.CreateWBSPhase.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.QAWBS);
            this.CreateWBSPhase.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.CreateWBSPhase.ToolTip = null;
            this.CreateWBSPhase.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.CreateWBSPhase.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CreateWBSPhase_Execute);
            // 
            // GetResource
            // 
            this.GetResource.Caption = "Get Resource Info";
            this.GetResource.ConfirmationMessage = null;
            this.GetResource.Id = "Get_Resource";
            this.GetResource.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.Resources);
            this.GetResource.ToolTip = null;
            this.GetResource.TypeOfView = typeof(DevExpress.ExpressApp.View);
            this.GetResource.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.GetResource_Execute);
            // 
            // Calculate_AHT_for_ticket
            // 
            this.Calculate_AHT_for_ticket.Caption = "Calculate AHTforticket";
            this.Calculate_AHT_for_ticket.ConfirmationMessage = null;
            this.Calculate_AHT_for_ticket.Id = "CalculateAHTforticket";
            this.Calculate_AHT_for_ticket.NullValuePrompt = null;
            this.Calculate_AHT_for_ticket.ShortCaption = " Calculate  AHT for this ticket";
            this.Calculate_AHT_for_ticket.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TicketAHT);
            this.Calculate_AHT_for_ticket.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.Calculate_AHT_for_ticket.ToolTip = null;
            this.Calculate_AHT_for_ticket.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.Calculate_AHT_for_ticket.Execute += new DevExpress.ExpressApp.Actions.ParametrizedActionExecuteEventHandler(this.Calculate_AHT_for_ticket_Execute);
            // 
            // AHTForCompletedTickets
            // 
            this.AHTForCompletedTickets.Caption = "AHT For Completed Tickets";
            this.AHTForCompletedTickets.ConfirmationMessage = null;
            this.AHTForCompletedTickets.Id = "AHTForCompletedTickets";
            this.AHTForCompletedTickets.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AverageHandleTime);
            this.AHTForCompletedTickets.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.AHTForCompletedTickets.ToolTip = null;
            this.AHTForCompletedTickets.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.AHTForCompletedTickets.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.AHTForCompletedTickets_Execute);
            // 
            // AHTForNotCompletedTickets
            // 
            this.AHTForNotCompletedTickets.Caption = "AHT For Not Completed Tickets";
            this.AHTForNotCompletedTickets.ConfirmationMessage = null;
            this.AHTForNotCompletedTickets.Id = "AHTForNotCompletedTickets";
            this.AHTForNotCompletedTickets.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AverageHandleTime);
            this.AHTForNotCompletedTickets.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.AHTForNotCompletedTickets.ToolTip = null;
            this.AHTForNotCompletedTickets.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.AHTForNotCompletedTickets.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.AHTForNotCompletedTickets_Execute);
            // 
            // AutoTask_Integration
            // 
            this.Actions.Add(this.creatproject);
            this.Actions.Add(this.updateproject);
            this.Actions.Add(this.download_AHt_file);
            this.Actions.Add(this.calculate_dot);
            this.Actions.Add(this.CreateTrackingtaskonautotask);
            this.Actions.Add(this.Calculate_AHT);
            this.Actions.Add(this.ShowTaskNotes);
            this.Actions.Add(this.DOT_download);
            this.Actions.Add(this.Get_DOT_data);
            this.Actions.Add(this.CreateWBSPhase);
            this.Actions.Add(this.GetResource);
            this.Actions.Add(this.Calculate_AHT_for_ticket);
            this.Actions.Add(this.AHTForCompletedTickets);
            this.Actions.Add(this.AHTForNotCompletedTickets);
            this.Activated += new System.EventHandler(this.AutoTask_Integration_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction GetResource;
        private DevExpress.ExpressApp.Actions.SimpleAction creatproject;
        private DevExpress.ExpressApp.Actions.SimpleAction updateproject;
        private DevExpress.ExpressApp.Actions.SimpleAction Calculate_AHT;
        private DevExpress.ExpressApp.Actions.SimpleAction download_AHt_file;
        private DevExpress.ExpressApp.Actions.ParametrizedAction Calculate_AHT_for_ticket;
        private DevExpress.ExpressApp.Actions.SimpleAction calculate_dot;
        private DevExpress.ExpressApp.Actions.PopupWindowShowAction ShowTaskNotes;
        private DevExpress.ExpressApp.Actions.SimpleAction DOT_download;
        private DevExpress.ExpressApp.Actions.SimpleAction Get_DOT_data;
        private DevExpress.ExpressApp.Actions.SimpleAction CreateWBSPhase;
        private DevExpress.ExpressApp.Actions.SimpleAction CreateTrackingtaskonautotask;
        private DevExpress.ExpressApp.Actions.SimpleAction AHTForCompletedTickets;
        private DevExpress.ExpressApp.Actions.SimpleAction AHTForNotCompletedTickets;
    }
}
