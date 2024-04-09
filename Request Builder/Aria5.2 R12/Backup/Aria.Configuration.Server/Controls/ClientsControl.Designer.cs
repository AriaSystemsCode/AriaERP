namespace Aria.Configuration.Server.Controls
{
    partial class ClientsControl
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
            this.buttonCreateUpdateDatabase = new System.Windows.Forms.Button();
            this.labelClientsSetup = new System.Windows.Forms.Label();
            this.labelNotes = new System.Windows.Forms.Label();
            this.cLIENTSBindingSource = new System.Windows.Forms.BindingSource(this.components);
            this._System_MasterDataSet = new Aria.Configuration.Server._System_MasterDataSet();
            this.cLIENTSTableAdapter = new Aria.Configuration.Server._System_MasterDataSetTableAdapters.CLIENTSTableAdapter();
            this.dataGridViewClients = new System.Windows.Forms.DataGridView();
            this.buttonAttach = new System.Windows.Forms.Button();
            this.errorProviderDatabaseNotUpdate = new System.Windows.Forms.ErrorProvider(this.components);
            this.buttonUnassign = new System.Windows.Forms.Button();
            this.Select = new System.Windows.Forms.DataGridViewCheckBoxColumn();
            this.CCLIENTID = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.CCLIENTNAME = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.REQSERVER = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.DBStatus = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Aria27Access = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Aria4XPAccess = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.DatabaseCurrentVersion = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.DatabaseTargetVersion = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.CustomizationCurrentVersion = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.CustomizationTargetVersion = new System.Windows.Forms.DataGridViewTextBoxColumn();
            ((System.ComponentModel.ISupportInitialize)(this.cLIENTSBindingSource)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this._System_MasterDataSet)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewClients)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderDatabaseNotUpdate)).BeginInit();
            this.SuspendLayout();
            // 
            // buttonCreateUpdateDatabase
            // 
            this.buttonCreateUpdateDatabase.Location = new System.Drawing.Point(269, 340);
            this.buttonCreateUpdateDatabase.Name = "buttonCreateUpdateDatabase";
            this.buttonCreateUpdateDatabase.Size = new System.Drawing.Size(244, 23);
            this.buttonCreateUpdateDatabase.TabIndex = 3;
            this.buttonCreateUpdateDatabase.Text = "Create / Upgrate Database";
            this.buttonCreateUpdateDatabase.UseVisualStyleBackColor = true;
            this.buttonCreateUpdateDatabase.Click += new System.EventHandler(this.buttonCreateUpdateDatabase_Click);
            // 
            // labelClientsSetup
            // 
            this.labelClientsSetup.AutoSize = true;
            this.labelClientsSetup.BackColor = System.Drawing.Color.Transparent;
            this.labelClientsSetup.Font = new System.Drawing.Font("Tahoma", 12.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelClientsSetup.Location = new System.Drawing.Point(8, 13);
            this.labelClientsSetup.Name = "labelClientsSetup";
            this.labelClientsSetup.Size = new System.Drawing.Size(123, 21);
            this.labelClientsSetup.TabIndex = 0;
            this.labelClientsSetup.Text = "Clients Setup";
            // 
            // labelNotes
            // 
            this.labelNotes.AutoSize = true;
            this.labelNotes.Location = new System.Drawing.Point(32, 39);
            this.labelNotes.Name = "labelNotes";
            this.labelNotes.Size = new System.Drawing.Size(466, 13);
            this.labelNotes.TabIndex = 1;
            this.labelNotes.Text = "This screen allows you to know the clients statuses and assgin each client to his" +
                " request server.";
            // 
            // cLIENTSBindingSource
            // 
            this.cLIENTSBindingSource.DataMember = "CLIENTS";
            this.cLIENTSBindingSource.DataSource = this._System_MasterDataSet;
            // 
            // _System_MasterDataSet
            // 
            this._System_MasterDataSet.DataSetName = "_System_MasterDataSet";
            this._System_MasterDataSet.SchemaSerializationMode = System.Data.SchemaSerializationMode.IncludeSchema;
            // 
            // cLIENTSTableAdapter
            // 
            this.cLIENTSTableAdapter.ClearBeforeFill = true;
            // 
            // dataGridViewClients
            // 
            this.dataGridViewClients.AllowUserToAddRows = false;
            this.dataGridViewClients.AllowUserToDeleteRows = false;
            this.dataGridViewClients.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridViewClients.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Select,
            this.CCLIENTID,
            this.CCLIENTNAME,
            this.REQSERVER,
            this.DBStatus,
            this.Aria27Access,
            this.Aria4XPAccess,
            this.DatabaseCurrentVersion,
            this.DatabaseTargetVersion,
            this.CustomizationCurrentVersion,
            this.CustomizationTargetVersion});
            this.dataGridViewClients.Location = new System.Drawing.Point(35, 64);
            this.dataGridViewClients.Name = "dataGridViewClients";
            this.dataGridViewClients.Size = new System.Drawing.Size(478, 270);
            this.dataGridViewClients.TabIndex = 2;
            this.dataGridViewClients.CellPainting += new System.Windows.Forms.DataGridViewCellPaintingEventHandler(this.dataGridViewClients_CellPainting);
            // 
            // buttonAttach
            // 
            this.buttonAttach.Location = new System.Drawing.Point(269, 369);
            this.buttonAttach.Name = "buttonAttach";
            this.buttonAttach.Size = new System.Drawing.Size(244, 23);
            this.buttonAttach.TabIndex = 4;
            this.buttonAttach.Text = "Assign this client Requests to this Server";
            this.buttonAttach.UseVisualStyleBackColor = true;
            this.buttonAttach.Click += new System.EventHandler(this.buttonAttach_Click);
            // 
            // errorProviderDatabaseNotUpdate
            // 
            this.errorProviderDatabaseNotUpdate.ContainerControl = this;
            // 
            // buttonUnassign
            // 
            this.buttonUnassign.Location = new System.Drawing.Point(269, 398);
            this.buttonUnassign.Name = "buttonUnassign";
            this.buttonUnassign.Size = new System.Drawing.Size(244, 23);
            this.buttonUnassign.TabIndex = 5;
            this.buttonUnassign.Text = "Unassign this client Requests from this Server";
            this.buttonUnassign.UseVisualStyleBackColor = true;
            this.buttonUnassign.Click += new System.EventHandler(this.buttonUnassign_Click);
            // 
            // Select
            // 
            this.Select.DataPropertyName = "Select";
            this.Select.HeaderText = "";
            this.Select.Name = "Select";
            this.Select.Width = 20;
            // 
            // CCLIENTID
            // 
            this.CCLIENTID.DataPropertyName = "CCLIENTID";
            this.CCLIENTID.HeaderText = "Client ID";
            this.CCLIENTID.Name = "CCLIENTID";
            this.CCLIENTID.ReadOnly = true;
            this.CCLIENTID.Width = 75;
            // 
            // CCLIENTNAME
            // 
            this.CCLIENTNAME.DataPropertyName = "CCLIENTNAME";
            this.CCLIENTNAME.HeaderText = "Client Name";
            this.CCLIENTNAME.Name = "CCLIENTNAME";
            this.CCLIENTNAME.ReadOnly = true;
            this.CCLIENTNAME.Width = 95;
            // 
            // REQSERVER
            // 
            this.REQSERVER.DataPropertyName = "REQSERVER";
            this.REQSERVER.HeaderText = "Request Server";
            this.REQSERVER.Name = "REQSERVER";
            this.REQSERVER.ReadOnly = true;
            this.REQSERVER.Width = 120;
            // 
            // DBStatus
            // 
            this.DBStatus.DataPropertyName = "DBStatus";
            this.DBStatus.HeaderText = "Database Status";
            this.DBStatus.Name = "DBStatus";
            this.DBStatus.ReadOnly = true;
            this.DBStatus.Width = 150;
            // 
            // Aria27Access
            // 
            this.Aria27Access.DataPropertyName = "Aria27Access";
            this.Aria27Access.HeaderText = "Aria 27 Accessibility";
            this.Aria27Access.Name = "Aria27Access";
            this.Aria27Access.ReadOnly = true;
            this.Aria27Access.Width = 150;
            // 
            // Aria4XPAccess
            // 
            this.Aria4XPAccess.DataPropertyName = "Aria4XPAccess";
            this.Aria4XPAccess.HeaderText = "Aria4XP Accessibility";
            this.Aria4XPAccess.Name = "Aria4XPAccess";
            this.Aria4XPAccess.ReadOnly = true;
            this.Aria4XPAccess.Width = 150;
            // 
            // DatabaseCurrentVersion
            // 
            this.DatabaseCurrentVersion.DataPropertyName = "DatabaseCurrentVersion";
            this.DatabaseCurrentVersion.HeaderText = "Database Current Version";
            this.DatabaseCurrentVersion.Name = "DatabaseCurrentVersion";
            this.DatabaseCurrentVersion.ReadOnly = true;
            this.DatabaseCurrentVersion.Width = 180;
            // 
            // DatabaseTargetVersion
            // 
            this.DatabaseTargetVersion.DataPropertyName = "DatabaseTargetVersion";
            this.DatabaseTargetVersion.HeaderText = "Database Target Version";
            this.DatabaseTargetVersion.Name = "DatabaseTargetVersion";
            this.DatabaseTargetVersion.ReadOnly = true;
            this.DatabaseTargetVersion.Width = 150;
            // 
            // CustomizationCurrentVersion
            // 
            this.CustomizationCurrentVersion.DataPropertyName = "CustomizationCurrentVersion";
            this.CustomizationCurrentVersion.HeaderText = "Customization Current Version";
            this.CustomizationCurrentVersion.Name = "CustomizationCurrentVersion";
            this.CustomizationCurrentVersion.ReadOnly = true;
            this.CustomizationCurrentVersion.Width = 180;
            // 
            // CustomizationTargetVersion
            // 
            this.CustomizationTargetVersion.DataPropertyName = "CustomizationTargetVersion";
            this.CustomizationTargetVersion.HeaderText = "Customization Target Version";
            this.CustomizationTargetVersion.Name = "CustomizationTargetVersion";
            this.CustomizationTargetVersion.ReadOnly = true;
            this.CustomizationTargetVersion.Width = 180;
            // 
            // ClientsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.buttonUnassign);
            this.Controls.Add(this.buttonAttach);
            this.Controls.Add(this.dataGridViewClients);
            this.Controls.Add(this.labelClientsSetup);
            this.Controls.Add(this.labelNotes);
            this.Controls.Add(this.buttonCreateUpdateDatabase);
            this.Name = "ClientsControl";
            this.Size = new System.Drawing.Size(536, 435);
            this.Load += new System.EventHandler(this.ClientsControl_Load);
            ((System.ComponentModel.ISupportInitialize)(this.cLIENTSBindingSource)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this._System_MasterDataSet)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewClients)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderDatabaseNotUpdate)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button buttonCreateUpdateDatabase;
        private System.Windows.Forms.Label labelClientsSetup;
        private System.Windows.Forms.Label labelNotes;
        private _System_MasterDataSet _System_MasterDataSet;
        private System.Windows.Forms.BindingSource cLIENTSBindingSource;
        private Aria.Configuration.Server._System_MasterDataSetTableAdapters.CLIENTSTableAdapter cLIENTSTableAdapter;
        private System.Windows.Forms.DataGridView dataGridViewClients;
        private System.Windows.Forms.Button buttonAttach;
        private System.Windows.Forms.ErrorProvider errorProviderDatabaseNotUpdate;
        private System.Windows.Forms.Button buttonUnassign;
        private System.Windows.Forms.DataGridViewCheckBoxColumn Select;
        private System.Windows.Forms.DataGridViewTextBoxColumn CCLIENTID;
        private System.Windows.Forms.DataGridViewTextBoxColumn CCLIENTNAME;
        private System.Windows.Forms.DataGridViewTextBoxColumn REQSERVER;
        private System.Windows.Forms.DataGridViewTextBoxColumn DBStatus;
        private System.Windows.Forms.DataGridViewTextBoxColumn Aria27Access;
        private System.Windows.Forms.DataGridViewTextBoxColumn Aria4XPAccess;
        private System.Windows.Forms.DataGridViewTextBoxColumn DatabaseCurrentVersion;
        private System.Windows.Forms.DataGridViewTextBoxColumn DatabaseTargetVersion;
        private System.Windows.Forms.DataGridViewTextBoxColumn CustomizationCurrentVersion;
        private System.Windows.Forms.DataGridViewTextBoxColumn CustomizationTargetVersion;
    }
}
