using AriaObjectBrowser;
using AriaObjectBrowser.UserControls;

namespace AriaObjectBrowser.Forms
{
    partial class AriaObjectBrowserMain
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

            ChangeClipboardChain(this.Handle, nextClipboardViewer);

            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AriaObjectBrowserMain));
            this.menuStrip = new System.Windows.Forms.MenuStrip();
            this.fileMenuToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.printToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.propertiesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editMenuToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.cutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.copyToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.pasteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
            this.editToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator12 = new System.Windows.Forms.ToolStripSeparator();
            this.deleteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.renameToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.changeNotesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator6 = new System.Windows.Forms.ToolStripSeparator();
            this.markAsDefaultToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.viewMenuToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.thumbnailsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.filmstripToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.singleFileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator7 = new System.Windows.Forms.ToolStripSeparator();
            this.taskPaneToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.sortByToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.sortByFileNameToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.sortByDateToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.sortByFileTypeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.sortByFileSizeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileNamesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator8 = new System.Windows.Forms.ToolStripSeparator();
            this.refreshToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.helpMenuToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.formTableLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
            this.standardToolStrip = new System.Windows.Forms.ToolStrip();
            this.printToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator10 = new System.Windows.Forms.ToolStripSeparator();
            this.cutToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.copyToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.pasteToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator13 = new System.Windows.Forms.ToolStripSeparator();
            this.editToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator9 = new System.Windows.Forms.ToolStripSeparator();
            this.deleteToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator11 = new System.Windows.Forms.ToolStripSeparator();
            this.markAsDefaultToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator14 = new System.Windows.Forms.ToolStripSeparator();
            this.zoomToolStripComboBox = new System.Windows.Forms.ToolStripComboBox();
            this.splitContainerRight = new System.Windows.Forms.SplitContainer();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.viewToolStrip = new System.Windows.Forms.ToolStrip();
            this.thumbnailsToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.filmstripToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.singleFileToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.zoomPanel = new System.Windows.Forms.Panel();
            this.zoomInButton = new System.Windows.Forms.PictureBox();
            this.zoomOutButton = new System.Windows.Forms.PictureBox();
            this.zoomLine1 = new System.Windows.Forms.GroupBox();
            this.moveNextButton = new System.Windows.Forms.Button();
            this.movePreviousButton = new System.Windows.Forms.Button();
            this.trackBar = new System.Windows.Forms.TrackBar();
            this.splitContainerLeft = new System.Windows.Forms.SplitContainer();
            this.objectsContainer = new AriaObjectBrowser.UserControls.AriaObjectsContainer();
            this.singleFileControl = new AriaObjectBrowser.UserControls.AriaSingleFileControl();
            this.taskPane = new TaskPaneControl.TaskPane();
            this.propertiesTaskPanePage = new TaskPaneControl.TaskPanePage();
            this.propertiesFileLocation = new System.Windows.Forms.Label();
            this.propertiesFileNotes = new System.Windows.Forms.TextBox();
            this.propertiesLine2 = new System.Windows.Forms.GroupBox();
            this.propertiesNoteLabel = new System.Windows.Forms.Label();
            this.propertiesLine1 = new System.Windows.Forms.GroupBox();
            this.propertiesFileLocationLabel = new System.Windows.Forms.Label();
            this.propertiesFileModifiedLabel = new System.Windows.Forms.Label();
            this.propertiesFileSizeLabel = new System.Windows.Forms.Label();
            this.propertiesFileTypeLabel = new System.Windows.Forms.Label();
            this.propertiesFilePropertiesLabel = new System.Windows.Forms.Label();
            this.propertiesFileNameLabel = new System.Windows.Forms.Label();
            this.changeNotesTaskPanePage = new TaskPaneControl.TaskPanePage();
            this.changeNotesNotes = new System.Windows.Forms.TextBox();
            this.changeNotesOk = new System.Windows.Forms.Button();
            this.changeNotesLine1 = new System.Windows.Forms.GroupBox();
            this.changeNotesNotesLabel = new System.Windows.Forms.Label();
            this.changeNotesFileNameLabel = new System.Windows.Forms.Label();
            this.renameTaskPanePage = new TaskPaneControl.TaskPanePage();
            this.renameFileName = new System.Windows.Forms.TextBox();
            this.renameOk = new System.Windows.Forms.Button();
            this.renameLine1 = new System.Windows.Forms.GroupBox();
            this.renameRenameLabel = new System.Windows.Forms.Label();
            this.renameFileNameLabel = new System.Windows.Forms.Label();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.menuStrip.SuspendLayout();
            this.formTableLayoutPanel.SuspendLayout();
            this.standardToolStrip.SuspendLayout();
            this.splitContainerRight.Panel1.SuspendLayout();
            this.splitContainerRight.Panel2.SuspendLayout();
            this.splitContainerRight.SuspendLayout();
            this.tableLayoutPanel1.SuspendLayout();
            this.viewToolStrip.SuspendLayout();
            this.zoomPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.zoomInButton)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.zoomOutButton)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.trackBar)).BeginInit();
            this.splitContainerLeft.Panel1.SuspendLayout();
            this.splitContainerLeft.Panel2.SuspendLayout();
            this.splitContainerLeft.SuspendLayout();
            this.taskPane.SuspendLayout();
            this.propertiesTaskPanePage.SuspendLayout();
            this.changeNotesTaskPanePage.SuspendLayout();
            this.renameTaskPanePage.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip
            // 
            this.menuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileMenuToolStripMenuItem,
            this.editMenuToolStripMenuItem,
            this.viewMenuToolStripMenuItem,
            this.helpMenuToolStripMenuItem});
            this.menuStrip.Location = new System.Drawing.Point(0, 0);
            this.menuStrip.Name = "menuStrip";
            this.menuStrip.Size = new System.Drawing.Size(729, 24);
            this.menuStrip.TabIndex = 1;
            this.menuStrip.Text = "menuStrip1";
            // 
            // fileMenuToolStripMenuItem
            // 
            this.fileMenuToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.addToolStripMenuItem,
            this.toolStripSeparator1,
            this.printToolStripMenuItem,
            this.toolStripSeparator3,
            this.propertiesToolStripMenuItem,
            this.toolStripSeparator4,
            this.exitToolStripMenuItem});
            this.fileMenuToolStripMenuItem.Name = "fileMenuToolStripMenuItem";
            this.fileMenuToolStripMenuItem.Size = new System.Drawing.Size(35, 20);
            this.fileMenuToolStripMenuItem.Text = "&File";
            // 
            // addToolStripMenuItem
            // 
            this.addToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("addToolStripMenuItem.Image")));
            this.addToolStripMenuItem.Name = "addToolStripMenuItem";
            this.addToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.A)));
            this.addToolStripMenuItem.Size = new System.Drawing.Size(146, 22);
            this.addToolStripMenuItem.Text = "Add...";
            this.addToolStripMenuItem.Click += new System.EventHandler(this.addToolStripMenuItem_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(143, 6);
            // 
            // printToolStripMenuItem
            // 
            this.printToolStripMenuItem.Enabled = false;
            this.printToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("printToolStripMenuItem.Image")));
            this.printToolStripMenuItem.Name = "printToolStripMenuItem";
            this.printToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.P)));
            this.printToolStripMenuItem.Size = new System.Drawing.Size(146, 22);
            this.printToolStripMenuItem.Text = "Print...";
            this.printToolStripMenuItem.Click += new System.EventHandler(this.printToolStripMenuItem_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(143, 6);
            // 
            // propertiesToolStripMenuItem
            // 
            this.propertiesToolStripMenuItem.Name = "propertiesToolStripMenuItem";
            this.propertiesToolStripMenuItem.Size = new System.Drawing.Size(146, 22);
            this.propertiesToolStripMenuItem.Text = "Properties";
            this.propertiesToolStripMenuItem.Click += new System.EventHandler(this.propertiesToolStripMenuItem_Click);
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(143, 6);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("exitToolStripMenuItem.Image")));
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(146, 22);
            this.exitToolStripMenuItem.Text = "Exit";
            this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
            // 
            // editMenuToolStripMenuItem
            // 
            this.editMenuToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.cutToolStripMenuItem,
            this.copyToolStripMenuItem,
            this.pasteToolStripMenuItem,
            this.toolStripSeparator5,
            this.editToolStripMenuItem,
            this.toolStripSeparator12,
            this.deleteToolStripMenuItem,
            this.renameToolStripMenuItem,
            this.changeNotesToolStripMenuItem,
            this.toolStripSeparator6,
            this.markAsDefaultToolStripMenuItem});
            this.editMenuToolStripMenuItem.Name = "editMenuToolStripMenuItem";
            this.editMenuToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.editMenuToolStripMenuItem.Text = "&Edit";
            // 
            // cutToolStripMenuItem
            // 
            this.cutToolStripMenuItem.Enabled = false;
            this.cutToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("cutToolStripMenuItem.Image")));
            this.cutToolStripMenuItem.Name = "cutToolStripMenuItem";
            this.cutToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.X)));
            this.cutToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.cutToolStripMenuItem.Text = "Cut";
            this.cutToolStripMenuItem.Click += new System.EventHandler(this.cutToolStripMenuItem_Click);
            // 
            // copyToolStripMenuItem
            // 
            this.copyToolStripMenuItem.Enabled = false;
            this.copyToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("copyToolStripMenuItem.Image")));
            this.copyToolStripMenuItem.Name = "copyToolStripMenuItem";
            this.copyToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.C)));
            this.copyToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.copyToolStripMenuItem.Text = "Copy";
            this.copyToolStripMenuItem.Click += new System.EventHandler(this.copyToolStripMenuItem_Click);
            // 
            // pasteToolStripMenuItem
            // 
            this.pasteToolStripMenuItem.Enabled = false;
            this.pasteToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("pasteToolStripMenuItem.Image")));
            this.pasteToolStripMenuItem.Name = "pasteToolStripMenuItem";
            this.pasteToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.V)));
            this.pasteToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.pasteToolStripMenuItem.Text = "Paste";
            this.pasteToolStripMenuItem.Click += new System.EventHandler(this.pasteToolStripMenuItem_Click);
            // 
            // toolStripSeparator5
            // 
            this.toolStripSeparator5.Name = "toolStripSeparator5";
            this.toolStripSeparator5.Size = new System.Drawing.Size(151, 6);
            // 
            // editToolStripMenuItem
            // 
            this.editToolStripMenuItem.Enabled = false;
            this.editToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("editToolStripMenuItem.Image")));
            this.editToolStripMenuItem.Name = "editToolStripMenuItem";
            this.editToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.editToolStripMenuItem.Text = "Edit...";
            this.editToolStripMenuItem.Click += new System.EventHandler(this.editToolStripMenuItem_Click);
            // 
            // toolStripSeparator12
            // 
            this.toolStripSeparator12.Name = "toolStripSeparator12";
            this.toolStripSeparator12.Size = new System.Drawing.Size(151, 6);
            // 
            // deleteToolStripMenuItem
            // 
            this.deleteToolStripMenuItem.Enabled = false;
            this.deleteToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("deleteToolStripMenuItem.Image")));
            this.deleteToolStripMenuItem.Name = "deleteToolStripMenuItem";
            this.deleteToolStripMenuItem.ShortcutKeys = System.Windows.Forms.Keys.Delete;
            this.deleteToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.deleteToolStripMenuItem.Text = "Delete";
            this.deleteToolStripMenuItem.Click += new System.EventHandler(this.deleteToolStripMenuItem_Click);
            // 
            // renameToolStripMenuItem
            // 
            this.renameToolStripMenuItem.Enabled = false;
            this.renameToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("renameToolStripMenuItem.Image")));
            this.renameToolStripMenuItem.Name = "renameToolStripMenuItem";
            this.renameToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.renameToolStripMenuItem.Text = "Rename...";
            this.renameToolStripMenuItem.Click += new System.EventHandler(this.renameToolStripMenuItem_Click);
            // 
            // changeNotesToolStripMenuItem
            // 
            this.changeNotesToolStripMenuItem.Enabled = false;
            this.changeNotesToolStripMenuItem.Name = "changeNotesToolStripMenuItem";
            this.changeNotesToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.changeNotesToolStripMenuItem.Text = "Change Notes...";
            this.changeNotesToolStripMenuItem.Click += new System.EventHandler(this.changeNotesToolStripMenuItem_Click);
            // 
            // toolStripSeparator6
            // 
            this.toolStripSeparator6.Name = "toolStripSeparator6";
            this.toolStripSeparator6.Size = new System.Drawing.Size(151, 6);
            // 
            // markAsDefaultToolStripMenuItem
            // 
            this.markAsDefaultToolStripMenuItem.Enabled = false;
            this.markAsDefaultToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("markAsDefaultToolStripMenuItem.Image")));
            this.markAsDefaultToolStripMenuItem.Name = "markAsDefaultToolStripMenuItem";
            this.markAsDefaultToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.markAsDefaultToolStripMenuItem.Text = "Mark As Default";
            this.markAsDefaultToolStripMenuItem.Click += new System.EventHandler(this.markAsDefaultToolStripMenuItem_Click);
            // 
            // viewMenuToolStripMenuItem
            // 
            this.viewMenuToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.thumbnailsToolStripMenuItem,
            this.filmstripToolStripMenuItem,
            this.singleFileToolStripMenuItem,
            this.toolStripSeparator7,
            this.taskPaneToolStripMenuItem,
            this.sortByToolStripMenuItem,
            this.showFileNamesToolStripMenuItem,
            this.toolStripSeparator8,
            this.refreshToolStripMenuItem});
            this.viewMenuToolStripMenuItem.Name = "viewMenuToolStripMenuItem";
            this.viewMenuToolStripMenuItem.Size = new System.Drawing.Size(41, 20);
            this.viewMenuToolStripMenuItem.Text = "&View";
            // 
            // thumbnailsToolStripMenuItem
            // 
            this.thumbnailsToolStripMenuItem.Checked = true;
            this.thumbnailsToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.thumbnailsToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("thumbnailsToolStripMenuItem.Image")));
            this.thumbnailsToolStripMenuItem.Name = "thumbnailsToolStripMenuItem";
            this.thumbnailsToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.T)));
            this.thumbnailsToolStripMenuItem.Size = new System.Drawing.Size(165, 22);
            this.thumbnailsToolStripMenuItem.Text = "Thumbnails";
            this.thumbnailsToolStripMenuItem.Click += new System.EventHandler(this.thumbnailsToolStripMenuItem_Click);
            // 
            // filmstripToolStripMenuItem
            // 
            this.filmstripToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("filmstripToolStripMenuItem.Image")));
            this.filmstripToolStripMenuItem.Name = "filmstripToolStripMenuItem";
            this.filmstripToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.B)));
            this.filmstripToolStripMenuItem.Size = new System.Drawing.Size(165, 22);
            this.filmstripToolStripMenuItem.Text = "Filmstrip";
            this.filmstripToolStripMenuItem.Click += new System.EventHandler(this.filmstripToolStripMenuItem_Click);
            // 
            // singleFileToolStripMenuItem
            // 
            this.singleFileToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("singleFileToolStripMenuItem.Image")));
            this.singleFileToolStripMenuItem.Name = "singleFileToolStripMenuItem";
            this.singleFileToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.G)));
            this.singleFileToolStripMenuItem.Size = new System.Drawing.Size(165, 22);
            this.singleFileToolStripMenuItem.Text = "Single File";
            this.singleFileToolStripMenuItem.Click += new System.EventHandler(this.singleFileToolStripMenuItem_Click);
            // 
            // toolStripSeparator7
            // 
            this.toolStripSeparator7.Name = "toolStripSeparator7";
            this.toolStripSeparator7.Size = new System.Drawing.Size(162, 6);
            // 
            // taskPaneToolStripMenuItem
            // 
            this.taskPaneToolStripMenuItem.Checked = true;
            this.taskPaneToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.taskPaneToolStripMenuItem.Name = "taskPaneToolStripMenuItem";
            this.taskPaneToolStripMenuItem.Size = new System.Drawing.Size(165, 22);
            this.taskPaneToolStripMenuItem.Text = "Task Pane";
            this.taskPaneToolStripMenuItem.Click += new System.EventHandler(this.taskPaneToolStripMenuItem_Click);
            // 
            // sortByToolStripMenuItem
            // 
            this.sortByToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.sortByFileNameToolStripMenuItem,
            this.sortByDateToolStripMenuItem,
            this.sortByFileTypeToolStripMenuItem,
            this.sortByFileSizeToolStripMenuItem});
            this.sortByToolStripMenuItem.Name = "sortByToolStripMenuItem";
            this.sortByToolStripMenuItem.Size = new System.Drawing.Size(165, 22);
            this.sortByToolStripMenuItem.Text = "Sort By";
            // 
            // sortByFileNameToolStripMenuItem
            // 
            this.sortByFileNameToolStripMenuItem.Checked = true;
            this.sortByFileNameToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.sortByFileNameToolStripMenuItem.Name = "sortByFileNameToolStripMenuItem";
            this.sortByFileNameToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
            this.sortByFileNameToolStripMenuItem.Text = "File Name";
            this.sortByFileNameToolStripMenuItem.Click += new System.EventHandler(this.sortByFileNameToolStripMenuItem_Click);
            // 
            // sortByDateToolStripMenuItem
            // 
            this.sortByDateToolStripMenuItem.Name = "sortByDateToolStripMenuItem";
            this.sortByDateToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
            this.sortByDateToolStripMenuItem.Text = "Date";
            this.sortByDateToolStripMenuItem.Click += new System.EventHandler(this.sortByDateToolStripMenuItem_Click);
            // 
            // sortByFileTypeToolStripMenuItem
            // 
            this.sortByFileTypeToolStripMenuItem.Name = "sortByFileTypeToolStripMenuItem";
            this.sortByFileTypeToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
            this.sortByFileTypeToolStripMenuItem.Text = "File Type";
            this.sortByFileTypeToolStripMenuItem.Click += new System.EventHandler(this.sortByFileTypeToolStripMenuItem_Click);
            // 
            // sortByFileSizeToolStripMenuItem
            // 
            this.sortByFileSizeToolStripMenuItem.Name = "sortByFileSizeToolStripMenuItem";
            this.sortByFileSizeToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
            this.sortByFileSizeToolStripMenuItem.Text = "File Size";
            this.sortByFileSizeToolStripMenuItem.Click += new System.EventHandler(this.sortByFileSizeToolStripMenuItem_Click);
            // 
            // showFileNamesToolStripMenuItem
            // 
            this.showFileNamesToolStripMenuItem.Checked = true;
            this.showFileNamesToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.showFileNamesToolStripMenuItem.Name = "showFileNamesToolStripMenuItem";
            this.showFileNamesToolStripMenuItem.Size = new System.Drawing.Size(165, 22);
            this.showFileNamesToolStripMenuItem.Text = "Show File Names";
            this.showFileNamesToolStripMenuItem.Click += new System.EventHandler(this.showFileNamesToolStripMenuItem_Click);
            // 
            // toolStripSeparator8
            // 
            this.toolStripSeparator8.Name = "toolStripSeparator8";
            this.toolStripSeparator8.Size = new System.Drawing.Size(162, 6);
            // 
            // refreshToolStripMenuItem
            // 
            this.refreshToolStripMenuItem.Name = "refreshToolStripMenuItem";
            this.refreshToolStripMenuItem.ShortcutKeys = System.Windows.Forms.Keys.F5;
            this.refreshToolStripMenuItem.Size = new System.Drawing.Size(165, 22);
            this.refreshToolStripMenuItem.Text = "Refresh";
            this.refreshToolStripMenuItem.Click += new System.EventHandler(this.refreshToolStripMenuItem_Click);
            // 
            // helpMenuToolStripMenuItem
            // 
            this.helpMenuToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.aboutToolStripMenuItem});
            this.helpMenuToolStripMenuItem.Name = "helpMenuToolStripMenuItem";
            this.helpMenuToolStripMenuItem.Size = new System.Drawing.Size(40, 20);
            this.helpMenuToolStripMenuItem.Text = "&Help";
            // 
            // aboutToolStripMenuItem
            // 
            this.aboutToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("aboutToolStripMenuItem.Image")));
            this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
            this.aboutToolStripMenuItem.Size = new System.Drawing.Size(115, 22);
            this.aboutToolStripMenuItem.Text = "About...";
            this.aboutToolStripMenuItem.Click += new System.EventHandler(this.aboutToolStripMenuItem_Click);
            // 
            // formTableLayoutPanel
            // 
            this.formTableLayoutPanel.ColumnCount = 1;
            this.formTableLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.formTableLayoutPanel.Controls.Add(this.standardToolStrip, 0, 0);
            this.formTableLayoutPanel.Controls.Add(this.splitContainerRight, 0, 1);
            this.formTableLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.formTableLayoutPanel.Location = new System.Drawing.Point(0, 24);
            this.formTableLayoutPanel.Name = "formTableLayoutPanel";
            this.formTableLayoutPanel.RowCount = 2;
            this.formTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 25F));
            this.formTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 295F));
            this.formTableLayoutPanel.Size = new System.Drawing.Size(729, 478);
            this.formTableLayoutPanel.TabIndex = 6;
            // 
            // standardToolStrip
            // 
            this.standardToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.printToolStripButton,
            this.toolStripSeparator10,
            this.cutToolStripButton,
            this.copyToolStripButton,
            this.pasteToolStripButton,
            this.toolStripSeparator13,
            this.editToolStripButton,
            this.toolStripSeparator9,
            this.deleteToolStripButton,
            this.toolStripSeparator11,
            this.markAsDefaultToolStripButton,
            this.toolStripSeparator14,
            this.zoomToolStripComboBox});
            this.standardToolStrip.Location = new System.Drawing.Point(0, 0);
            this.standardToolStrip.Name = "standardToolStrip";
            this.standardToolStrip.Size = new System.Drawing.Size(729, 25);
            this.standardToolStrip.TabIndex = 6;
            this.standardToolStrip.Text = "toolStrip1";
            // 
            // printToolStripButton
            // 
            this.printToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.printToolStripButton.Enabled = false;
            this.printToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("printToolStripButton.Image")));
            this.printToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.printToolStripButton.Name = "printToolStripButton";
            this.printToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.printToolStripButton.Text = "Print...";
            this.printToolStripButton.Click += new System.EventHandler(this.printToolStripButton_Click);
            // 
            // toolStripSeparator10
            // 
            this.toolStripSeparator10.Name = "toolStripSeparator10";
            this.toolStripSeparator10.Size = new System.Drawing.Size(6, 25);
            // 
            // cutToolStripButton
            // 
            this.cutToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.cutToolStripButton.Enabled = false;
            this.cutToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("cutToolStripButton.Image")));
            this.cutToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.cutToolStripButton.Name = "cutToolStripButton";
            this.cutToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.cutToolStripButton.Text = "Cut";
            this.cutToolStripButton.Click += new System.EventHandler(this.cutToolStripButton_Click);
            // 
            // copyToolStripButton
            // 
            this.copyToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.copyToolStripButton.Enabled = false;
            this.copyToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("copyToolStripButton.Image")));
            this.copyToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.copyToolStripButton.Name = "copyToolStripButton";
            this.copyToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.copyToolStripButton.Text = "Copy";
            this.copyToolStripButton.Click += new System.EventHandler(this.copyToolStripButton_Click);
            // 
            // pasteToolStripButton
            // 
            this.pasteToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.pasteToolStripButton.Enabled = false;
            this.pasteToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("pasteToolStripButton.Image")));
            this.pasteToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.pasteToolStripButton.Name = "pasteToolStripButton";
            this.pasteToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.pasteToolStripButton.Text = "Paste";
            this.pasteToolStripButton.Click += new System.EventHandler(this.pasteToolStripButton_Click);
            // 
            // toolStripSeparator13
            // 
            this.toolStripSeparator13.Name = "toolStripSeparator13";
            this.toolStripSeparator13.Size = new System.Drawing.Size(6, 25);
            // 
            // editToolStripButton
            // 
            this.editToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.editToolStripButton.Enabled = false;
            this.editToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("editToolStripButton.Image")));
            this.editToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.editToolStripButton.Name = "editToolStripButton";
            this.editToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.editToolStripButton.Text = "Edit";
            this.editToolStripButton.Click += new System.EventHandler(this.editToolStripButton_Click);
            // 
            // toolStripSeparator9
            // 
            this.toolStripSeparator9.Name = "toolStripSeparator9";
            this.toolStripSeparator9.Size = new System.Drawing.Size(6, 25);
            // 
            // deleteToolStripButton
            // 
            this.deleteToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.deleteToolStripButton.Enabled = false;
            this.deleteToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("deleteToolStripButton.Image")));
            this.deleteToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.deleteToolStripButton.Name = "deleteToolStripButton";
            this.deleteToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.deleteToolStripButton.Text = "Delete";
            this.deleteToolStripButton.Click += new System.EventHandler(this.deleteToolStripButton_Click);
            // 
            // toolStripSeparator11
            // 
            this.toolStripSeparator11.Name = "toolStripSeparator11";
            this.toolStripSeparator11.Size = new System.Drawing.Size(6, 25);
            // 
            // markAsDefaultToolStripButton
            // 
            this.markAsDefaultToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.markAsDefaultToolStripButton.Enabled = false;
            this.markAsDefaultToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("markAsDefaultToolStripButton.Image")));
            this.markAsDefaultToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.markAsDefaultToolStripButton.Name = "markAsDefaultToolStripButton";
            this.markAsDefaultToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.markAsDefaultToolStripButton.Text = "Mark As Default";
            this.markAsDefaultToolStripButton.Click += new System.EventHandler(this.markAsDefaultToolStripButton_Click);
            // 
            // toolStripSeparator14
            // 
            this.toolStripSeparator14.Name = "toolStripSeparator14";
            this.toolStripSeparator14.Size = new System.Drawing.Size(6, 25);
            // 
            // zoomToolStripComboBox
            // 
            this.zoomToolStripComboBox.Items.AddRange(new object[] {
            "800%",
            "400%",
            "200%",
            "150%",
            "100%",
            "75%",
            "50%",
            "25%",
            "12%"});
            this.zoomToolStripComboBox.MaxDropDownItems = 15;
            this.zoomToolStripComboBox.Name = "zoomToolStripComboBox";
            this.zoomToolStripComboBox.Size = new System.Drawing.Size(75, 25);
            this.zoomToolStripComboBox.Text = "100%";
            this.zoomToolStripComboBox.TextChanged += new System.EventHandler(this.zoomToolStripComboBox_TextChanged);
            // 
            // splitContainerRight
            // 
            this.splitContainerRight.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainerRight.Location = new System.Drawing.Point(3, 28);
            this.splitContainerRight.Name = "splitContainerRight";
            // 
            // splitContainerRight.Panel1
            // 
            this.splitContainerRight.Panel1.Controls.Add(this.tableLayoutPanel1);
            // 
            // splitContainerRight.Panel2
            // 
            this.splitContainerRight.Panel2.Controls.Add(this.taskPane);
            this.splitContainerRight.Size = new System.Drawing.Size(723, 447);
            this.splitContainerRight.SplitterDistance = 500;
            this.splitContainerRight.TabIndex = 7;
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(227)))), ((int)(((byte)(239)))), ((int)(((byte)(255)))));
            this.tableLayoutPanel1.ColumnCount = 1;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 500F));
            this.tableLayoutPanel1.Controls.Add(this.viewToolStrip, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.zoomPanel, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.splitContainerLeft, 0, 1);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 3;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 22F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 37F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(500, 447);
            this.tableLayoutPanel1.TabIndex = 0;
            // 
            // viewToolStrip
            // 
            this.viewToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.viewToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.thumbnailsToolStripButton,
            this.filmstripToolStripButton,
            this.singleFileToolStripButton});
            this.viewToolStrip.Location = new System.Drawing.Point(0, 0);
            this.viewToolStrip.Name = "viewToolStrip";
            this.viewToolStrip.Size = new System.Drawing.Size(500, 22);
            this.viewToolStrip.TabIndex = 0;
            this.viewToolStrip.Text = "toolStrip1";
            // 
            // thumbnailsToolStripButton
            // 
            this.thumbnailsToolStripButton.Checked = true;
            this.thumbnailsToolStripButton.CheckState = System.Windows.Forms.CheckState.Checked;
            this.thumbnailsToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.thumbnailsToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("thumbnailsToolStripButton.Image")));
            this.thumbnailsToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.thumbnailsToolStripButton.Name = "thumbnailsToolStripButton";
            this.thumbnailsToolStripButton.Size = new System.Drawing.Size(23, 19);
            this.thumbnailsToolStripButton.Text = "Thumbnails";
            this.thumbnailsToolStripButton.Click += new System.EventHandler(this.thumbnailsToolStripButton_Click);
            // 
            // filmstripToolStripButton
            // 
            this.filmstripToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.filmstripToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("filmstripToolStripButton.Image")));
            this.filmstripToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.filmstripToolStripButton.Name = "filmstripToolStripButton";
            this.filmstripToolStripButton.Size = new System.Drawing.Size(23, 19);
            this.filmstripToolStripButton.Text = "Filmstrip";
            this.filmstripToolStripButton.Click += new System.EventHandler(this.filmstripToolStripButton_Click);
            // 
            // singleFileToolStripButton
            // 
            this.singleFileToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.singleFileToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("singleFileToolStripButton.Image")));
            this.singleFileToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.singleFileToolStripButton.Name = "singleFileToolStripButton";
            this.singleFileToolStripButton.Size = new System.Drawing.Size(23, 19);
            this.singleFileToolStripButton.Text = "Single File";
            this.singleFileToolStripButton.Click += new System.EventHandler(this.singleFileToolStripButton_Click);
            // 
            // zoomPanel
            // 
            this.zoomPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(227)))), ((int)(((byte)(239)))), ((int)(((byte)(255)))));
            this.zoomPanel.Controls.Add(this.zoomInButton);
            this.zoomPanel.Controls.Add(this.zoomOutButton);
            this.zoomPanel.Controls.Add(this.zoomLine1);
            this.zoomPanel.Controls.Add(this.moveNextButton);
            this.zoomPanel.Controls.Add(this.movePreviousButton);
            this.zoomPanel.Controls.Add(this.trackBar);
            this.zoomPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.zoomPanel.Location = new System.Drawing.Point(3, 413);
            this.zoomPanel.Name = "zoomPanel";
            this.zoomPanel.Size = new System.Drawing.Size(494, 31);
            this.zoomPanel.TabIndex = 1;
            // 
            // zoomInButton
            // 
            this.zoomInButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.zoomInButton.Image = ((System.Drawing.Image)(resources.GetObject("zoomInButton.Image")));
            this.zoomInButton.Location = new System.Drawing.Point(466, 8);
            this.zoomInButton.Name = "zoomInButton";
            this.zoomInButton.Size = new System.Drawing.Size(21, 17);
            this.zoomInButton.TabIndex = 7;
            this.zoomInButton.TabStop = false;
            // 
            // zoomOutButton
            // 
            this.zoomOutButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.zoomOutButton.Image = ((System.Drawing.Image)(resources.GetObject("zoomOutButton.Image")));
            this.zoomOutButton.Location = new System.Drawing.Point(165, 8);
            this.zoomOutButton.Name = "zoomOutButton";
            this.zoomOutButton.Size = new System.Drawing.Size(21, 17);
            this.zoomOutButton.TabIndex = 6;
            this.zoomOutButton.TabStop = false;
            // 
            // zoomLine1
            // 
            this.zoomLine1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.zoomLine1.Location = new System.Drawing.Point(1, -4);
            this.zoomLine1.Name = "zoomLine1";
            this.zoomLine1.Size = new System.Drawing.Size(495, 8);
            this.zoomLine1.TabIndex = 5;
            this.zoomLine1.TabStop = false;
            // 
            // moveNextButton
            // 
            this.moveNextButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.moveNextButton.AutoEllipsis = true;
            this.moveNextButton.Enabled = false;
            this.moveNextButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.moveNextButton.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(227)))), ((int)(((byte)(239)))), ((int)(((byte)(255)))));
            this.moveNextButton.Image = ((System.Drawing.Image)(resources.GetObject("moveNextButton.Image")));
            this.moveNextButton.Location = new System.Drawing.Point(127, 4);
            this.moveNextButton.Name = "moveNextButton";
            this.moveNextButton.Size = new System.Drawing.Size(25, 23);
            this.moveNextButton.TabIndex = 2;
            this.moveNextButton.UseVisualStyleBackColor = true;
            this.moveNextButton.Click += new System.EventHandler(this.moveNextButton_Click);
            // 
            // movePreviousButton
            // 
            this.movePreviousButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.movePreviousButton.Enabled = false;
            this.movePreviousButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.movePreviousButton.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(227)))), ((int)(((byte)(239)))), ((int)(((byte)(255)))));
            this.movePreviousButton.Image = ((System.Drawing.Image)(resources.GetObject("movePreviousButton.Image")));
            this.movePreviousButton.Location = new System.Drawing.Point(104, 4);
            this.movePreviousButton.Name = "movePreviousButton";
            this.movePreviousButton.Size = new System.Drawing.Size(23, 23);
            this.movePreviousButton.TabIndex = 1;
            this.movePreviousButton.UseVisualStyleBackColor = true;
            this.movePreviousButton.Click += new System.EventHandler(this.movePreviousButton_Click);
            // 
            // trackBar
            // 
            this.trackBar.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.trackBar.LargeChange = 1;
            this.trackBar.Location = new System.Drawing.Point(184, 5);
            this.trackBar.Maximum = 800;
            this.trackBar.Minimum = 12;
            this.trackBar.Name = "trackBar";
            this.trackBar.Size = new System.Drawing.Size(284, 45);
            this.trackBar.TabIndex = 4;
            this.trackBar.TickStyle = System.Windows.Forms.TickStyle.None;
            this.trackBar.Value = 100;
            this.trackBar.Scroll += new System.EventHandler(this.trackBar_Scroll);
            // 
            // splitContainerLeft
            // 
            this.splitContainerLeft.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainerLeft.FixedPanel = System.Windows.Forms.FixedPanel.Panel1;
            this.splitContainerLeft.Location = new System.Drawing.Point(3, 25);
            this.splitContainerLeft.Name = "splitContainerLeft";
            // 
            // splitContainerLeft.Panel1
            // 
            this.splitContainerLeft.Panel1.Controls.Add(this.objectsContainer);
            // 
            // splitContainerLeft.Panel2
            // 
            this.splitContainerLeft.Panel2.Controls.Add(this.singleFileControl);
            this.splitContainerLeft.Panel2Collapsed = true;
            this.splitContainerLeft.Size = new System.Drawing.Size(494, 382);
            this.splitContainerLeft.SplitterDistance = 164;
            this.splitContainerLeft.TabIndex = 2;
            // 
            // objectsContainer
            // 
            this.objectsContainer.AriaApplication = null;
            this.objectsContainer.AriaObjects = null;
            this.objectsContainer.AutoScroll = true;
            this.objectsContainer.AutoSize = true;
            this.objectsContainer.BackColor = System.Drawing.Color.White;
            this.objectsContainer.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center;
            this.objectsContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.objectsContainer.Location = new System.Drawing.Point(0, 0);
            this.objectsContainer.Name = "objectsContainer";
            this.objectsContainer.Settings = null;
            this.objectsContainer.Size = new System.Drawing.Size(494, 382);
            this.objectsContainer.TabIndex = 4;
            this.objectsContainer.Enter += new System.EventHandler(this.objectsContainer_Enter);
            // 
            // singleFileControl
            // 
            this.singleFileControl.AriaApplication = null;
            this.singleFileControl.AutoScroll = true;
            this.singleFileControl.BackColor = System.Drawing.Color.Transparent;
            this.singleFileControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.singleFileControl.Location = new System.Drawing.Point(0, 0);
            this.singleFileControl.Name = "singleFileControl";
            this.singleFileControl.Settings = null;
            this.singleFileControl.Size = new System.Drawing.Size(96, 100);
            this.singleFileControl.TabIndex = 5;
            this.singleFileControl.Visible = false;
            this.singleFileControl.Enter += new System.EventHandler(this.singleFileControl_Enter);
            // 
            // taskPane
            // 
            this.taskPane.BackColor = System.Drawing.Color.White;
            this.taskPane.Controls.Add(this.propertiesTaskPanePage);
            this.taskPane.Controls.Add(this.changeNotesTaskPanePage);
            this.taskPane.Controls.Add(this.renameTaskPanePage);
            this.taskPane.Dock = System.Windows.Forms.DockStyle.Fill;
            this.taskPane.Location = new System.Drawing.Point(0, 0);
            this.taskPane.Name = "taskPane";
            this.taskPane.NavigationPanelAppearance.Style = TaskPaneControl.DisplayStyle.Office2003;
            this.taskPane.SelectedIndex = 0;
            this.taskPane.SelectedPage = this.propertiesTaskPanePage;
            this.taskPane.ShowImageColumnInDropdown = true;
            this.taskPane.Size = new System.Drawing.Size(219, 447);
            this.taskPane.TabIndex = 4;
            this.taskPane.TaskPanePages.Add(this.propertiesTaskPanePage);
            this.taskPane.TaskPanePages.Add(this.renameTaskPanePage);
            this.taskPane.TaskPanePages.Add(this.changeNotesTaskPanePage);
            this.taskPane.TaskPaneCloseClick += new TaskPaneControl.TaskPane.TaskPaneCloseClickEventHandler(this.taskPane_TaskPaneCloseClick);
            // 
            // propertiesTaskPanePage
            // 
            this.propertiesTaskPanePage.AutoScroll = true;
            this.propertiesTaskPanePage.Caption = "Properties";
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFileLocation);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFileNotes);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesLine2);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesNoteLabel);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesLine1);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFileLocationLabel);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFileModifiedLabel);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFileSizeLabel);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFileTypeLabel);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFilePropertiesLabel);
            this.propertiesTaskPanePage.Controls.Add(this.propertiesFileNameLabel);
            this.propertiesTaskPanePage.CornerRadius = new Ascend.CornerRadius(0, 0, 8, 8);
            this.propertiesTaskPanePage.Dock = System.Windows.Forms.DockStyle.Fill;
            this.propertiesTaskPanePage.GradientLowColor = System.Drawing.Color.FromArgb(((int)(((byte)(221)))), ((int)(((byte)(236)))), ((int)(((byte)(254)))));
            this.propertiesTaskPanePage.Key = "";
            this.propertiesTaskPanePage.Location = new System.Drawing.Point(0, 25);
            this.propertiesTaskPanePage.Name = "propertiesTaskPanePage";
            this.propertiesTaskPanePage.Size = new System.Drawing.Size(219, 422);
            // 
            // propertiesFileLocation
            // 
            this.propertiesFileLocation.AutoSize = true;
            this.propertiesFileLocation.Location = new System.Drawing.Point(76, 145);
            this.propertiesFileLocation.Name = "propertiesFileLocation";
            this.propertiesFileLocation.Size = new System.Drawing.Size(0, 13);
            this.propertiesFileLocation.TabIndex = 11;
            // 
            // propertiesFileNotes
            // 
            this.propertiesFileNotes.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.propertiesFileNotes.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(221)))), ((int)(((byte)(236)))), ((int)(((byte)(254)))));
            this.propertiesFileNotes.Location = new System.Drawing.Point(9, 205);
            this.propertiesFileNotes.Multiline = true;
            this.propertiesFileNotes.Name = "propertiesFileNotes";
            this.propertiesFileNotes.ReadOnly = true;
            this.propertiesFileNotes.Size = new System.Drawing.Size(200, 142);
            this.propertiesFileNotes.TabIndex = 5;
            // 
            // propertiesLine2
            // 
            this.propertiesLine2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.propertiesLine2.BackColor = System.Drawing.Color.Transparent;
            this.propertiesLine2.Location = new System.Drawing.Point(9, 191);
            this.propertiesLine2.Name = "propertiesLine2";
            this.propertiesLine2.Size = new System.Drawing.Size(201, 8);
            this.propertiesLine2.TabIndex = 10;
            this.propertiesLine2.TabStop = false;
            // 
            // propertiesNoteLabel
            // 
            this.propertiesNoteLabel.AutoSize = true;
            this.propertiesNoteLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.propertiesNoteLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.propertiesNoteLabel.Location = new System.Drawing.Point(18, 172);
            this.propertiesNoteLabel.Name = "propertiesNoteLabel";
            this.propertiesNoteLabel.Size = new System.Drawing.Size(40, 13);
            this.propertiesNoteLabel.TabIndex = 9;
            this.propertiesNoteLabel.Text = "Notes";
            // 
            // propertiesLine1
            // 
            this.propertiesLine1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.propertiesLine1.BackColor = System.Drawing.Color.Transparent;
            this.propertiesLine1.Location = new System.Drawing.Point(10, 84);
            this.propertiesLine1.Name = "propertiesLine1";
            this.propertiesLine1.Size = new System.Drawing.Size(201, 8);
            this.propertiesLine1.TabIndex = 8;
            this.propertiesLine1.TabStop = false;
            // 
            // propertiesFileLocationLabel
            // 
            this.propertiesFileLocationLabel.AutoSize = true;
            this.propertiesFileLocationLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.propertiesFileLocationLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.propertiesFileLocationLabel.Location = new System.Drawing.Point(19, 145);
            this.propertiesFileLocationLabel.Name = "propertiesFileLocationLabel";
            this.propertiesFileLocationLabel.Size = new System.Drawing.Size(51, 13);
            this.propertiesFileLocationLabel.TabIndex = 7;
            this.propertiesFileLocationLabel.Text = "Location:";
            // 
            // propertiesFileModifiedLabel
            // 
            this.propertiesFileModifiedLabel.AutoSize = true;
            this.propertiesFileModifiedLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.propertiesFileModifiedLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.propertiesFileModifiedLabel.Location = new System.Drawing.Point(19, 130);
            this.propertiesFileModifiedLabel.Name = "propertiesFileModifiedLabel";
            this.propertiesFileModifiedLabel.Size = new System.Drawing.Size(50, 13);
            this.propertiesFileModifiedLabel.TabIndex = 6;
            this.propertiesFileModifiedLabel.Text = "Modified:";
            // 
            // propertiesFileSizeLabel
            // 
            this.propertiesFileSizeLabel.AutoSize = true;
            this.propertiesFileSizeLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.propertiesFileSizeLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.propertiesFileSizeLabel.Location = new System.Drawing.Point(19, 116);
            this.propertiesFileSizeLabel.Name = "propertiesFileSizeLabel";
            this.propertiesFileSizeLabel.Size = new System.Drawing.Size(30, 13);
            this.propertiesFileSizeLabel.TabIndex = 5;
            this.propertiesFileSizeLabel.Text = "Size:";
            // 
            // propertiesFileTypeLabel
            // 
            this.propertiesFileTypeLabel.AutoSize = true;
            this.propertiesFileTypeLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.propertiesFileTypeLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.propertiesFileTypeLabel.Location = new System.Drawing.Point(19, 100);
            this.propertiesFileTypeLabel.Name = "propertiesFileTypeLabel";
            this.propertiesFileTypeLabel.Size = new System.Drawing.Size(34, 13);
            this.propertiesFileTypeLabel.TabIndex = 4;
            this.propertiesFileTypeLabel.Text = "Type:";
            // 
            // propertiesFilePropertiesLabel
            // 
            this.propertiesFilePropertiesLabel.AutoSize = true;
            this.propertiesFilePropertiesLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.propertiesFilePropertiesLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.propertiesFilePropertiesLabel.Location = new System.Drawing.Point(16, 65);
            this.propertiesFilePropertiesLabel.Name = "propertiesFilePropertiesLabel";
            this.propertiesFilePropertiesLabel.Size = new System.Drawing.Size(88, 13);
            this.propertiesFilePropertiesLabel.TabIndex = 3;
            this.propertiesFilePropertiesLabel.Text = "File Properties";
            // 
            // propertiesFileNameLabel
            // 
            this.propertiesFileNameLabel.AutoSize = true;
            this.propertiesFileNameLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.propertiesFileNameLabel.Location = new System.Drawing.Point(13, 35);
            this.propertiesFileNameLabel.Name = "propertiesFileNameLabel";
            this.propertiesFileNameLabel.Size = new System.Drawing.Size(101, 13);
            this.propertiesFileNameLabel.TabIndex = 2;
            this.propertiesFileNameLabel.Text = "No Selected File";
            // 
            // changeNotesTaskPanePage
            // 
            this.changeNotesTaskPanePage.AutoScroll = true;
            this.changeNotesTaskPanePage.Caption = "Change Notes";
            this.changeNotesTaskPanePage.Controls.Add(this.changeNotesNotes);
            this.changeNotesTaskPanePage.Controls.Add(this.changeNotesOk);
            this.changeNotesTaskPanePage.Controls.Add(this.changeNotesLine1);
            this.changeNotesTaskPanePage.Controls.Add(this.changeNotesNotesLabel);
            this.changeNotesTaskPanePage.Controls.Add(this.changeNotesFileNameLabel);
            this.changeNotesTaskPanePage.CornerRadius = new Ascend.CornerRadius(0, 0, 8, 8);
            this.changeNotesTaskPanePage.Dock = System.Windows.Forms.DockStyle.Fill;
            this.changeNotesTaskPanePage.GradientLowColor = System.Drawing.Color.FromArgb(((int)(((byte)(221)))), ((int)(((byte)(236)))), ((int)(((byte)(254)))));
            this.changeNotesTaskPanePage.Key = "";
            this.changeNotesTaskPanePage.Location = new System.Drawing.Point(0, 25);
            this.changeNotesTaskPanePage.Name = "changeNotesTaskPanePage";
            this.changeNotesTaskPanePage.Size = new System.Drawing.Size(219, 422);
            // 
            // changeNotesNotes
            // 
            this.changeNotesNotes.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.changeNotesNotes.Enabled = false;
            this.changeNotesNotes.Location = new System.Drawing.Point(10, 97);
            this.changeNotesNotes.Multiline = true;
            this.changeNotesNotes.Name = "changeNotesNotes";
            this.changeNotesNotes.Size = new System.Drawing.Size(200, 131);
            this.changeNotesNotes.TabIndex = 18;
            this.changeNotesNotes.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.changeNotesNotes_KeyPress);
            // 
            // changeNotesOk
            // 
            this.changeNotesOk.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.changeNotesOk.Enabled = false;
            this.changeNotesOk.Location = new System.Drawing.Point(135, 234);
            this.changeNotesOk.Name = "changeNotesOk";
            this.changeNotesOk.Size = new System.Drawing.Size(75, 23);
            this.changeNotesOk.TabIndex = 17;
            this.changeNotesOk.Text = "OK";
            this.changeNotesOk.UseVisualStyleBackColor = true;
            this.changeNotesOk.Click += new System.EventHandler(this.changeNotes_Click);
            // 
            // changeNotesLine1
            // 
            this.changeNotesLine1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.changeNotesLine1.BackColor = System.Drawing.Color.Transparent;
            this.changeNotesLine1.Location = new System.Drawing.Point(10, 84);
            this.changeNotesLine1.Name = "changeNotesLine1";
            this.changeNotesLine1.Size = new System.Drawing.Size(201, 8);
            this.changeNotesLine1.TabIndex = 16;
            this.changeNotesLine1.TabStop = false;
            // 
            // changeNotesNotesLabel
            // 
            this.changeNotesNotesLabel.AutoSize = true;
            this.changeNotesNotesLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.changeNotesNotesLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.changeNotesNotesLabel.Location = new System.Drawing.Point(13, 65);
            this.changeNotesNotesLabel.Name = "changeNotesNotesLabel";
            this.changeNotesNotesLabel.Size = new System.Drawing.Size(40, 13);
            this.changeNotesNotesLabel.TabIndex = 15;
            this.changeNotesNotesLabel.Text = "Notes";
            // 
            // changeNotesFileNameLabel
            // 
            this.changeNotesFileNameLabel.AutoSize = true;
            this.changeNotesFileNameLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.changeNotesFileNameLabel.Location = new System.Drawing.Point(13, 35);
            this.changeNotesFileNameLabel.Name = "changeNotesFileNameLabel";
            this.changeNotesFileNameLabel.Size = new System.Drawing.Size(101, 13);
            this.changeNotesFileNameLabel.TabIndex = 14;
            this.changeNotesFileNameLabel.Text = "No Selected File";
            // 
            // renameTaskPanePage
            // 
            this.renameTaskPanePage.AutoScroll = true;
            this.renameTaskPanePage.Caption = "Rename";
            this.renameTaskPanePage.Controls.Add(this.renameFileName);
            this.renameTaskPanePage.Controls.Add(this.renameOk);
            this.renameTaskPanePage.Controls.Add(this.renameLine1);
            this.renameTaskPanePage.Controls.Add(this.renameRenameLabel);
            this.renameTaskPanePage.Controls.Add(this.renameFileNameLabel);
            this.renameTaskPanePage.CornerRadius = new Ascend.CornerRadius(0, 0, 8, 8);
            this.renameTaskPanePage.Dock = System.Windows.Forms.DockStyle.Fill;
            this.renameTaskPanePage.GradientLowColor = System.Drawing.Color.FromArgb(((int)(((byte)(221)))), ((int)(((byte)(236)))), ((int)(((byte)(254)))));
            this.renameTaskPanePage.Key = "";
            this.renameTaskPanePage.Location = new System.Drawing.Point(0, 25);
            this.renameTaskPanePage.Name = "renameTaskPanePage";
            this.renameTaskPanePage.Size = new System.Drawing.Size(219, 422);
            // 
            // renameFileName
            // 
            this.renameFileName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.renameFileName.Enabled = false;
            this.renameFileName.Location = new System.Drawing.Point(10, 97);
            this.renameFileName.Name = "renameFileName";
            this.renameFileName.Size = new System.Drawing.Size(200, 20);
            this.renameFileName.TabIndex = 13;
            this.renameFileName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.renameFileName_KeyPress);
            // 
            // renameOk
            // 
            this.renameOk.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.renameOk.Enabled = false;
            this.renameOk.Location = new System.Drawing.Point(135, 130);
            this.renameOk.Name = "renameOk";
            this.renameOk.Size = new System.Drawing.Size(75, 23);
            this.renameOk.TabIndex = 12;
            this.renameOk.Text = "OK";
            this.renameOk.UseVisualStyleBackColor = true;
            this.renameOk.Click += new System.EventHandler(this.renameFile_Click);
            // 
            // renameLine1
            // 
            this.renameLine1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.renameLine1.BackColor = System.Drawing.Color.Transparent;
            this.renameLine1.Location = new System.Drawing.Point(10, 84);
            this.renameLine1.Name = "renameLine1";
            this.renameLine1.Size = new System.Drawing.Size(201, 8);
            this.renameLine1.TabIndex = 11;
            this.renameLine1.TabStop = false;
            // 
            // renameRenameLabel
            // 
            this.renameRenameLabel.AutoSize = true;
            this.renameRenameLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.renameRenameLabel.ForeColor = System.Drawing.SystemColors.MenuHighlight;
            this.renameRenameLabel.Location = new System.Drawing.Point(14, 65);
            this.renameRenameLabel.Name = "renameRenameLabel";
            this.renameRenameLabel.Size = new System.Drawing.Size(53, 13);
            this.renameRenameLabel.TabIndex = 10;
            this.renameRenameLabel.Text = "Rename";
            // 
            // renameFileNameLabel
            // 
            this.renameFileNameLabel.AutoSize = true;
            this.renameFileNameLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.renameFileNameLabel.Location = new System.Drawing.Point(12, 35);
            this.renameFileNameLabel.Name = "renameFileNameLabel";
            this.renameFileNameLabel.Size = new System.Drawing.Size(101, 13);
            this.renameFileNameLabel.TabIndex = 9;
            this.renameFileNameLabel.Text = "No Selected File";
            // 
            // AriaObjectBrowserMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(227)))), ((int)(((byte)(239)))), ((int)(((byte)(255)))));
            this.ClientSize = new System.Drawing.Size(729, 502);
            this.Controls.Add(this.formTableLayoutPanel);
            this.Controls.Add(this.menuStrip);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.KeyPreview = true;
            this.MainMenuStrip = this.menuStrip;
            this.Name = "AriaObjectBrowserMain";
            this.Text = "Aria Object Browser";
            this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
            this.Load += new System.EventHandler(this.AriaObjectBrowserMain_Load);
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.AriaObjectBrowserMain_FormClosing);
            this.menuStrip.ResumeLayout(false);
            this.menuStrip.PerformLayout();
            this.formTableLayoutPanel.ResumeLayout(false);
            this.formTableLayoutPanel.PerformLayout();
            this.standardToolStrip.ResumeLayout(false);
            this.standardToolStrip.PerformLayout();
            this.splitContainerRight.Panel1.ResumeLayout(false);
            this.splitContainerRight.Panel2.ResumeLayout(false);
            this.splitContainerRight.ResumeLayout(false);
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.viewToolStrip.ResumeLayout(false);
            this.viewToolStrip.PerformLayout();
            this.zoomPanel.ResumeLayout(false);
            this.zoomPanel.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.zoomInButton)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.zoomOutButton)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.trackBar)).EndInit();
            this.splitContainerLeft.Panel1.ResumeLayout(false);
            this.splitContainerLeft.Panel1.PerformLayout();
            this.splitContainerLeft.Panel2.ResumeLayout(false);
            this.splitContainerLeft.ResumeLayout(false);
            this.taskPane.ResumeLayout(false);
            this.taskPane.PerformLayout();
            this.propertiesTaskPanePage.ResumeLayout(false);
            this.propertiesTaskPanePage.PerformLayout();
            this.changeNotesTaskPanePage.ResumeLayout(false);
            this.changeNotesTaskPanePage.PerformLayout();
            this.renameTaskPanePage.ResumeLayout(false);
            this.renameTaskPanePage.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip;
        private System.Windows.Forms.ToolStripMenuItem fileMenuToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripMenuItem printToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripMenuItem propertiesToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem editMenuToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem copyToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem cutToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem pasteToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator5;
        private System.Windows.Forms.ToolStripMenuItem deleteToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem renameToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem changeNotesToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator6;
        private System.Windows.Forms.ToolStripMenuItem viewMenuToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem thumbnailsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem filmstripToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem singleFileToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator7;
        private System.Windows.Forms.ToolStripMenuItem taskPaneToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem sortByToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem sortByFileNameToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem sortByDateToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem sortByFileTypeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem sortByFileSizeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem showFileNamesToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator8;
        private System.Windows.Forms.ToolStripMenuItem refreshToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem helpMenuToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem markAsDefaultToolStripMenuItem;
        private System.Windows.Forms.TableLayoutPanel formTableLayoutPanel;
        private System.Windows.Forms.SplitContainer splitContainerRight;
        private TaskPaneControl.TaskPane taskPane;
        private TaskPaneControl.TaskPanePage propertiesTaskPanePage;
        private System.Windows.Forms.Label propertiesFileLocationLabel;
        private System.Windows.Forms.Label propertiesFileModifiedLabel;
        private System.Windows.Forms.Label propertiesFileSizeLabel;
        private System.Windows.Forms.Label propertiesFileTypeLabel;
        private System.Windows.Forms.Label propertiesFilePropertiesLabel;
        private System.Windows.Forms.Label propertiesFileNameLabel;
        private TaskPaneControl.TaskPanePage renameTaskPanePage;
        private System.Windows.Forms.ToolStrip standardToolStrip;
        private System.Windows.Forms.GroupBox propertiesLine1;
        private System.Windows.Forms.GroupBox renameLine1;
        private System.Windows.Forms.Label renameRenameLabel;
        private System.Windows.Forms.Label renameFileNameLabel;
        private System.Windows.Forms.Button renameOk;
        private System.Windows.Forms.TextBox renameFileName;
        private TaskPaneControl.TaskPanePage changeNotesTaskPanePage;
        private System.Windows.Forms.TextBox changeNotesNotes;
        private System.Windows.Forms.Button changeNotesOk;
        private System.Windows.Forms.GroupBox changeNotesLine1;
        private System.Windows.Forms.Label changeNotesNotesLabel;
        private System.Windows.Forms.Label changeNotesFileNameLabel;
        private System.Windows.Forms.ToolStripButton printToolStripButton;
        private System.Windows.Forms.ToolStripButton markAsDefaultToolStripButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator10;
        private System.Windows.Forms.ToolStripButton cutToolStripButton;
        private System.Windows.Forms.ToolStripButton copyToolStripButton;
        private System.Windows.Forms.ToolStripButton pasteToolStripButton;
        private System.Windows.Forms.ToolStripButton deleteToolStripButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator11;
        private System.Windows.Forms.ToolStripComboBox zoomToolStripComboBox;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.TextBox propertiesFileNotes;
        private System.Windows.Forms.GroupBox propertiesLine2;
        private System.Windows.Forms.Label propertiesNoteLabel;
        private System.Windows.Forms.Label propertiesFileLocation;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator9;
        private System.Windows.Forms.ToolStripMenuItem editToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator12;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator13;
        private System.Windows.Forms.ToolStripButton editToolStripButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator14;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.ToolStrip viewToolStrip;
        private System.Windows.Forms.ToolStripButton thumbnailsToolStripButton;
        private System.Windows.Forms.ToolStripButton filmstripToolStripButton;
        private System.Windows.Forms.ToolStripButton singleFileToolStripButton;
        private System.Windows.Forms.Panel zoomPanel;
        private System.Windows.Forms.PictureBox zoomInButton;
        private System.Windows.Forms.PictureBox zoomOutButton;
        private System.Windows.Forms.GroupBox zoomLine1;
        private System.Windows.Forms.Button moveNextButton;
        private System.Windows.Forms.Button movePreviousButton;
        private System.Windows.Forms.TrackBar trackBar;
        private System.Windows.Forms.SplitContainer splitContainerLeft;
        private AriaObjectsContainer objectsContainer;
        private AriaSingleFileControl singleFileControl;
    }
}