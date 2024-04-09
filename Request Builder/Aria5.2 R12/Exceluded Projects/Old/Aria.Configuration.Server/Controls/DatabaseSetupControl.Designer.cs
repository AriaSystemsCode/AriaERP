namespace Aria.Configuration.Server.Controls
{
    partial class DatabaseSetupControl
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
            this.labelDatabaseSetup = new System.Windows.Forms.Label();
            this.labelNotes = new System.Windows.Forms.Label();
            this.panelMain = new System.Windows.Forms.Panel();
            this.buttonSave = new System.Windows.Forms.Button();
            this.panelDatabaseSetup = new System.Windows.Forms.Panel();
            this.buttonTestConection = new System.Windows.Forms.Button();
            this.buttonCreateUpdateDatabase = new System.Windows.Forms.Button();
            this.labelLoginType = new System.Windows.Forms.Label();
            this.labelPassword = new System.Windows.Forms.Label();
            this.labelUserName = new System.Windows.Forms.Label();
            this.textBoxPassword = new System.Windows.Forms.TextBox();
            this.textBoxUserName = new System.Windows.Forms.TextBox();
            this.comboBoxLoginType = new System.Windows.Forms.ComboBox();
            this.labelServerName = new System.Windows.Forms.Label();
            this.comboBoxServerName = new System.Windows.Forms.ComboBox();
            this.errorProviderDatabaseNotUpdated = new System.Windows.Forms.ErrorProvider(this.components);
            this.errorProviderCannotCaonnect = new System.Windows.Forms.ErrorProvider(this.components);
            this.panelMain.SuspendLayout();
            this.panelDatabaseSetup.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderDatabaseNotUpdated)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotCaonnect)).BeginInit();
            this.SuspendLayout();
            // 
            // labelDatabaseSetup
            // 
            this.labelDatabaseSetup.AutoSize = true;
            this.labelDatabaseSetup.BackColor = System.Drawing.Color.Transparent;
            this.labelDatabaseSetup.Font = new System.Drawing.Font("Tahoma", 12.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelDatabaseSetup.Location = new System.Drawing.Point(3, 1);
            this.labelDatabaseSetup.Name = "labelDatabaseSetup";
            this.labelDatabaseSetup.Size = new System.Drawing.Size(145, 21);
            this.labelDatabaseSetup.TabIndex = 0;
            this.labelDatabaseSetup.Text = "Database Setup";
            // 
            // labelNotes
            // 
            this.labelNotes.AutoSize = true;
            this.labelNotes.Location = new System.Drawing.Point(27, 27);
            this.labelNotes.Name = "labelNotes";
            this.labelNotes.Size = new System.Drawing.Size(409, 26);
            this.labelNotes.TabIndex = 1;
            this.labelNotes.Text = "Specify the sql server will be used to store the databse on, according to the cur" +
                "rent\r\ndatabase version.";
            // 
            // panelMain
            // 
            this.panelMain.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelMain.Controls.Add(this.buttonSave);
            this.panelMain.Controls.Add(this.panelDatabaseSetup);
            this.panelMain.Controls.Add(this.labelDatabaseSetup);
            this.panelMain.Controls.Add(this.labelNotes);
            this.panelMain.Location = new System.Drawing.Point(3, 3);
            this.panelMain.Name = "panelMain";
            this.panelMain.Size = new System.Drawing.Size(510, 419);
            this.panelMain.TabIndex = 50;
            // 
            // buttonSave
            // 
            this.buttonSave.Enabled = false;
            this.buttonSave.Location = new System.Drawing.Point(404, 390);
            this.buttonSave.Name = "buttonSave";
            this.buttonSave.Size = new System.Drawing.Size(103, 23);
            this.buttonSave.TabIndex = 14;
            this.buttonSave.Text = "Save";
            this.buttonSave.UseVisualStyleBackColor = true;
            this.buttonSave.Click += new System.EventHandler(this.buttonSave_Click);
            // 
            // panelDatabaseSetup
            // 
            this.panelDatabaseSetup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelDatabaseSetup.Controls.Add(this.buttonTestConection);
            this.panelDatabaseSetup.Controls.Add(this.buttonCreateUpdateDatabase);
            this.panelDatabaseSetup.Controls.Add(this.labelLoginType);
            this.panelDatabaseSetup.Controls.Add(this.labelPassword);
            this.panelDatabaseSetup.Controls.Add(this.labelUserName);
            this.panelDatabaseSetup.Controls.Add(this.textBoxPassword);
            this.panelDatabaseSetup.Controls.Add(this.textBoxUserName);
            this.panelDatabaseSetup.Controls.Add(this.comboBoxLoginType);
            this.panelDatabaseSetup.Controls.Add(this.labelServerName);
            this.panelDatabaseSetup.Controls.Add(this.comboBoxServerName);
            this.panelDatabaseSetup.Location = new System.Drawing.Point(16, 66);
            this.panelDatabaseSetup.Name = "panelDatabaseSetup";
            this.panelDatabaseSetup.Size = new System.Drawing.Size(458, 143);
            this.panelDatabaseSetup.TabIndex = 2;
            // 
            // buttonTestConection
            // 
            this.buttonTestConection.Location = new System.Drawing.Point(187, 83);
            this.buttonTestConection.Name = "buttonTestConection";
            this.buttonTestConection.Size = new System.Drawing.Size(244, 23);
            this.buttonTestConection.TabIndex = 8;
            this.buttonTestConection.Text = "Test Connection";
            this.buttonTestConection.UseVisualStyleBackColor = true;
            this.buttonTestConection.Click += new System.EventHandler(this.buttonTestConection_Click);
            // 
            // buttonCreateUpdateDatabase
            // 
            this.buttonCreateUpdateDatabase.Location = new System.Drawing.Point(187, 112);
            this.buttonCreateUpdateDatabase.Name = "buttonCreateUpdateDatabase";
            this.buttonCreateUpdateDatabase.Size = new System.Drawing.Size(244, 23);
            this.buttonCreateUpdateDatabase.TabIndex = 9;
            this.buttonCreateUpdateDatabase.Text = "Create / Upgrate Database";
            this.buttonCreateUpdateDatabase.UseVisualStyleBackColor = true;
            this.buttonCreateUpdateDatabase.Click += new System.EventHandler(this.buttonCreateUpdateDatabase_Click);
            // 
            // labelLoginType
            // 
            this.labelLoginType.AutoSize = true;
            this.labelLoginType.Location = new System.Drawing.Point(30, 37);
            this.labelLoginType.Name = "labelLoginType";
            this.labelLoginType.Size = new System.Drawing.Size(63, 13);
            this.labelLoginType.TabIndex = 2;
            this.labelLoginType.Text = "Login Type:";
            // 
            // labelPassword
            // 
            this.labelPassword.AutoSize = true;
            this.labelPassword.Location = new System.Drawing.Point(276, 61);
            this.labelPassword.Name = "labelPassword";
            this.labelPassword.Size = new System.Drawing.Size(57, 13);
            this.labelPassword.TabIndex = 6;
            this.labelPassword.Text = "Password:";
            // 
            // labelUserName
            // 
            this.labelUserName.AutoSize = true;
            this.labelUserName.Location = new System.Drawing.Point(30, 62);
            this.labelUserName.Name = "labelUserName";
            this.labelUserName.Size = new System.Drawing.Size(63, 13);
            this.labelUserName.TabIndex = 4;
            this.labelUserName.Text = "User Name:";
            // 
            // textBoxPassword
            // 
            this.textBoxPassword.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxPassword.Location = new System.Drawing.Point(339, 57);
            this.textBoxPassword.Name = "textBoxPassword";
            this.textBoxPassword.Size = new System.Drawing.Size(92, 20);
            this.textBoxPassword.TabIndex = 7;
            this.textBoxPassword.UseSystemPasswordChar = true;
            this.textBoxPassword.TextChanged += new System.EventHandler(this.textBoxPassword_TextChanged);
            // 
            // textBoxUserName
            // 
            this.textBoxUserName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxUserName.Location = new System.Drawing.Point(187, 58);
            this.textBoxUserName.Name = "textBoxUserName";
            this.textBoxUserName.Size = new System.Drawing.Size(83, 20);
            this.textBoxUserName.TabIndex = 5;
            this.textBoxUserName.TextChanged += new System.EventHandler(this.textBoxUserName_TextChanged);
            // 
            // comboBoxLoginType
            // 
            this.comboBoxLoginType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.comboBoxLoginType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxLoginType.FormattingEnabled = true;
            this.comboBoxLoginType.Items.AddRange(new object[] {
            "WindowAuthentication",
            "SqlServerAuthentication"});
            this.comboBoxLoginType.Location = new System.Drawing.Point(187, 31);
            this.comboBoxLoginType.Name = "comboBoxLoginType";
            this.comboBoxLoginType.Size = new System.Drawing.Size(245, 21);
            this.comboBoxLoginType.TabIndex = 3;
            this.comboBoxLoginType.SelectedIndexChanged += new System.EventHandler(this.comboBoxLoginType_SelectedIndexChanged);
            // 
            // labelServerName
            // 
            this.labelServerName.AutoSize = true;
            this.labelServerName.Location = new System.Drawing.Point(30, 10);
            this.labelServerName.Name = "labelServerName";
            this.labelServerName.Size = new System.Drawing.Size(73, 13);
            this.labelServerName.TabIndex = 0;
            this.labelServerName.Text = "Server Name:";
            // 
            // comboBoxServerName
            // 
            this.comboBoxServerName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.comboBoxServerName.FormattingEnabled = true;
            this.comboBoxServerName.Location = new System.Drawing.Point(187, 4);
            this.comboBoxServerName.Name = "comboBoxServerName";
            this.comboBoxServerName.Size = new System.Drawing.Size(245, 21);
            this.comboBoxServerName.TabIndex = 1;
            this.comboBoxServerName.SelectedIndexChanged += new System.EventHandler(this.comboBoxServerName_SelectedIndexChanged);
            this.comboBoxServerName.DropDown += new System.EventHandler(this.comboBoxServerName_DropDown);
            // 
            // errorProviderDatabaseNotUpdated
            // 
            this.errorProviderDatabaseNotUpdated.ContainerControl = this;
            // 
            // errorProviderCannotCaonnect
            // 
            this.errorProviderCannotCaonnect.ContainerControl = this;
            // 
            // DatabaseSetupControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.panelMain);
            this.Name = "DatabaseSetupControl";
            this.Size = new System.Drawing.Size(516, 425);
            this.panelMain.ResumeLayout(false);
            this.panelMain.PerformLayout();
            this.panelDatabaseSetup.ResumeLayout(false);
            this.panelDatabaseSetup.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderDatabaseNotUpdated)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotCaonnect)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label labelDatabaseSetup;
        private System.Windows.Forms.Label labelNotes;
        private System.Windows.Forms.Panel panelMain;
        private System.Windows.Forms.ComboBox comboBoxServerName;
        private System.Windows.Forms.Label labelServerName;
        private System.Windows.Forms.ErrorProvider errorProviderDatabaseNotUpdated;
        private System.Windows.Forms.Panel panelDatabaseSetup;
        private System.Windows.Forms.Label labelLoginType;
        private System.Windows.Forms.Label labelPassword;
        private System.Windows.Forms.Label labelUserName;
        private System.Windows.Forms.TextBox textBoxPassword;
        private System.Windows.Forms.TextBox textBoxUserName;
        private System.Windows.Forms.ComboBox comboBoxLoginType;
        private System.Windows.Forms.ErrorProvider errorProviderCannotCaonnect;
        private System.Windows.Forms.Button buttonCreateUpdateDatabase;
        private System.Windows.Forms.Button buttonTestConection;
        private System.Windows.Forms.Button buttonSave;
    }
}
