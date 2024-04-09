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
            this.buttonAttach = new System.Windows.Forms.Button();
            this.errorProviderDatabaseNotUpdate = new System.Windows.Forms.ErrorProvider(this.components);
            this.buttonUnassign = new System.Windows.Forms.Button();
            this.txtCExcNotfEmail = new System.Windows.Forms.TextBox();
            this.label16 = new System.Windows.Forms.Label();
            this.udCMaxRecPerReq = new System.Windows.Forms.NumericUpDown();
            this.label9 = new System.Windows.Forms.Label();
            this.udCMaxTime = new System.Windows.Forms.NumericUpDown();
            this.label8 = new System.Windows.Forms.Label();
            this.udCMaxPerAgent = new System.Windows.Forms.NumericUpDown();
            this.label10 = new System.Windows.Forms.Label();
            this.label14 = new System.Windows.Forms.Label();
            this.buttonCSave = new System.Windows.Forms.Button();
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.numericUpDown1 = new System.Windows.Forms.NumericUpDown();
            ((System.ComponentModel.ISupportInitialize)(this.cLIENTSBindingSource)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this._System_MasterDataSet)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewClients)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderDatabaseNotUpdate)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.udCMaxRecPerReq)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.udCMaxTime)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.udCMaxPerAgent)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDown1)).BeginInit();
            this.SuspendLayout();
            // 
            // buttonCreateUpdateDatabase
            // 
            this.buttonCreateUpdateDatabase.Location = new System.Drawing.Point(23, 274);
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
            this.labelNotes.Size = new System.Drawing.Size(458, 13);
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
            this.dataGridViewClients.Size = new System.Drawing.Size(478, 201);
            this.dataGridViewClients.TabIndex = 2;
            this.dataGridViewClients.CellPainting += new System.Windows.Forms.DataGridViewCellPaintingEventHandler(this.dataGridViewClients_CellPainting);
            this.dataGridViewClients.RowEnter += new System.Windows.Forms.DataGridViewCellEventHandler(this.dataGridViewClients_RowEnter);
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
            this.Aria27Access.Visible = false;
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
            // buttonAttach
            // 
            this.buttonAttach.Location = new System.Drawing.Point(279, 274);
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
            this.buttonUnassign.Location = new System.Drawing.Point(22, 300);
            this.buttonUnassign.Name = "buttonUnassign";
            this.buttonUnassign.Size = new System.Drawing.Size(244, 23);
            this.buttonUnassign.TabIndex = 5;
            this.buttonUnassign.Text = "Unassign this client Requests from this Server";
            this.buttonUnassign.UseVisualStyleBackColor = true;
            this.buttonUnassign.Click += new System.EventHandler(this.buttonUnassign_Click);
            // 
            // txtCExcNotfEmail
            // 
            this.txtCExcNotfEmail.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.txtCExcNotfEmail.Location = new System.Drawing.Point(279, 401);
            this.txtCExcNotfEmail.Name = "txtCExcNotfEmail";
            this.txtCExcNotfEmail.Size = new System.Drawing.Size(212, 20);
            this.txtCExcNotfEmail.TabIndex = 38;
            this.txtCExcNotfEmail.TextChanged += new System.EventHandler(this.txtCExcNotfEmail_TextChanged);
            // 
            // label16
            // 
            this.label16.AutoSize = true;
            this.label16.Location = new System.Drawing.Point(23, 401);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(154, 13);
            this.label16.TabIndex = 37;
            this.label16.Text = "Exceed Limit Notification Email:";
            // 
            // udCMaxRecPerReq
            // 
            this.udCMaxRecPerReq.Location = new System.Drawing.Point(279, 377);
            this.udCMaxRecPerReq.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.udCMaxRecPerReq.Name = "udCMaxRecPerReq";
            this.udCMaxRecPerReq.Size = new System.Drawing.Size(86, 20);
            this.udCMaxRecPerReq.TabIndex = 36;
            this.udCMaxRecPerReq.ValueChanged += new System.EventHandler(this.udCMaxRecPerReq_ValueChanged);
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(23, 378);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(181, 13);
            this.label9.TabIndex = 35;
            this.label9.Text = "Max Number Of Records Per Report:";
            // 
            // udCMaxTime
            // 
            this.udCMaxTime.Location = new System.Drawing.Point(279, 353);
            this.udCMaxTime.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.udCMaxTime.Name = "udCMaxTime";
            this.udCMaxTime.Size = new System.Drawing.Size(86, 20);
            this.udCMaxTime.TabIndex = 34;
            this.udCMaxTime.ValueChanged += new System.EventHandler(this.udCMaxTime_ValueChanged);
            this.udCMaxTime.Leave += new System.EventHandler(this.udCMaxTime_Leave);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(23, 355);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(168, 13);
            this.label8.TabIndex = 33;
            this.label8.Text = "Max Execution Time Per Request:";
            // 
            // udCMaxPerAgent
            // 
            this.udCMaxPerAgent.Location = new System.Drawing.Point(279, 329);
            this.udCMaxPerAgent.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.udCMaxPerAgent.Name = "udCMaxPerAgent";
            this.udCMaxPerAgent.Size = new System.Drawing.Size(86, 20);
            this.udCMaxPerAgent.TabIndex = 32;
            this.udCMaxPerAgent.ValueChanged += new System.EventHandler(this.udCMaxPerAgent_ValueChanged);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(23, 331);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(220, 13);
            this.label10.TabIndex = 31;
            this.label10.Text = "Max Number Of Records Per Agent Request:";
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(386, 331);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(105, 13);
            this.label14.TabIndex = 39;
            this.label14.Text = "Set zero for unlimited";
            // 
            // buttonCSave
            // 
            this.buttonCSave.Enabled = false;
            this.buttonCSave.Location = new System.Drawing.Point(390, 359);
            this.buttonCSave.Name = "buttonCSave";
            this.buttonCSave.Size = new System.Drawing.Size(96, 23);
            this.buttonCSave.TabIndex = 40;
            this.buttonCSave.Text = "Save";
            this.buttonCSave.UseVisualStyleBackColor = true;
            this.buttonCSave.Click += new System.EventHandler(this.buttonCSave_Click);
            // 
            // textBox1
            // 
            this.textBox1.Location = new System.Drawing.Point(279, 303);
            this.textBox1.Name = "textBox1";
            this.textBox1.Size = new System.Drawing.Size(100, 20);
            this.textBox1.TabIndex = 41;
            this.textBox1.TextChanged += new System.EventHandler(this.textBox1_TextChanged);
            // 
            // numericUpDown1
            // 
            this.numericUpDown1.Location = new System.Drawing.Point(210, 399);
            this.numericUpDown1.Name = "numericUpDown1";
            this.numericUpDown1.Size = new System.Drawing.Size(44, 20);
            this.numericUpDown1.TabIndex = 42;
            this.numericUpDown1.Value = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.numericUpDown1.ValueChanged += new System.EventHandler(this.numericUpDown1_ValueChanged);
            // 
            // ClientsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.numericUpDown1);
            this.Controls.Add(this.textBox1);
            this.Controls.Add(this.label14);
            this.Controls.Add(this.buttonCSave);
            this.Controls.Add(this.txtCExcNotfEmail);
            this.Controls.Add(this.label16);
            this.Controls.Add(this.udCMaxRecPerReq);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.udCMaxTime);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.udCMaxPerAgent);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.buttonUnassign);
            this.Controls.Add(this.buttonAttach);
            this.Controls.Add(this.dataGridViewClients);
            this.Controls.Add(this.labelClientsSetup);
            this.Controls.Add(this.labelNotes);
            this.Controls.Add(this.buttonCreateUpdateDatabase);
            this.Name = "ClientsControl";
            this.Size = new System.Drawing.Size(536, 435);
            ((System.ComponentModel.ISupportInitialize)(this.cLIENTSBindingSource)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this._System_MasterDataSet)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewClients)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderDatabaseNotUpdate)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.udCMaxRecPerReq)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.udCMaxTime)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.udCMaxPerAgent)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDown1)).EndInit();
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
        private System.Windows.Forms.TextBox txtCExcNotfEmail;
        private System.Windows.Forms.Label label16;
        private System.Windows.Forms.NumericUpDown udCMaxRecPerReq;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.NumericUpDown udCMaxTime;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.NumericUpDown udCMaxPerAgent;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.Button buttonCSave;
        private System.Windows.Forms.NumericUpDown numericUpDown1;
        private System.Windows.Forms.TextBox textBox1;
    }
}
