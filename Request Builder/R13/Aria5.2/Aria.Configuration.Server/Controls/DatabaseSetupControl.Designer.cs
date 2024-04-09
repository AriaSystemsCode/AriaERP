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
            this.panel1 = new System.Windows.Forms.Panel();
            this.label5 = new System.Windows.Forms.Label();
            this.label15 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.comboBoxStartType = new System.Windows.Forms.ComboBox();
            this.label7 = new System.Windows.Forms.Label();
            this.buttonChangeAccount = new System.Windows.Forms.Button();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.textBoxServicePassword = new System.Windows.Forms.TextBox();
            this.textBoxServiceUserName = new System.Windows.Forms.TextBox();
            this.comboBoxAccountType = new System.Windows.Forms.ComboBox();
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
            this.label1 = new System.Windows.Forms.Label();
            this.textBoxCurrentVersion = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.textBoxTargetVersion = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.panelMain.SuspendLayout();
            this.panel1.SuspendLayout();
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
            this.labelDatabaseSetup.Location = new System.Drawing.Point(16, 0);
            this.labelDatabaseSetup.Name = "labelDatabaseSetup";
            this.labelDatabaseSetup.Size = new System.Drawing.Size(213, 21);
            this.labelDatabaseSetup.TabIndex = 0;
            this.labelDatabaseSetup.Text = "Database Configuration";
            // 
            // labelNotes
            // 
            this.labelNotes.AutoSize = true;
            this.labelNotes.Location = new System.Drawing.Point(27, 27);
            this.labelNotes.Name = "labelNotes";
            this.labelNotes.Size = new System.Drawing.Size(286, 13);
            this.labelNotes.TabIndex = 1;
            this.labelNotes.Text = "Specify the sql server will be used to store the databases.";
            // 
            // panelMain
            // 
            this.panelMain.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelMain.Controls.Add(this.panel1);
            this.panelMain.Controls.Add(this.buttonSave);
            this.panelMain.Controls.Add(this.panelDatabaseSetup);
            this.panelMain.Controls.Add(this.labelDatabaseSetup);
            this.panelMain.Controls.Add(this.labelNotes);
            this.panelMain.Location = new System.Drawing.Point(3, 3);
            this.panelMain.Name = "panelMain";
            this.panelMain.Size = new System.Drawing.Size(510, 419);
            this.panelMain.TabIndex = 0;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.label5);
            this.panel1.Controls.Add(this.label15);
            this.panel1.Controls.Add(this.label2);
            this.panel1.Controls.Add(this.comboBoxStartType);
            this.panel1.Controls.Add(this.label7);
            this.panel1.Controls.Add(this.buttonChangeAccount);
            this.panel1.Controls.Add(this.label3);
            this.panel1.Controls.Add(this.label4);
            this.panel1.Controls.Add(this.textBoxServicePassword);
            this.panel1.Controls.Add(this.textBoxServiceUserName);
            this.panel1.Controls.Add(this.comboBoxAccountType);
            this.panel1.Location = new System.Drawing.Point(20, 249);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(491, 140);
            this.panel1.TabIndex = 15;
            this.panel1.Visible = false;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.BackColor = System.Drawing.Color.Transparent;
            this.label5.Font = new System.Drawing.Font("Tahoma", 12.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(-4, 0);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(232, 21);
            this.label5.TabIndex = 0;
            this.label5.Text = "Database Service Account";
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
            this.label15.Location = new System.Drawing.Point(4, 170);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(418, 26);
            this.label15.TabIndex = 10;
            this.label15.Text = "Notes: Make sure the SQL Server Windows service run on a user which has access to" +
                " \r\nall Aria folders.";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(18, 56);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(77, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Account Type:";
            // 
            // comboBoxStartType
            // 
            this.comboBoxStartType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.comboBoxStartType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxStartType.FormattingEnabled = true;
            this.comboBoxStartType.Items.AddRange(new object[] {
            "Manual",
            "Disabled",
            "Automatic"});
            this.comboBoxStartType.Location = new System.Drawing.Point(187, 31);
            this.comboBoxStartType.Name = "comboBoxStartType";
            this.comboBoxStartType.Size = new System.Drawing.Size(244, 21);
            this.comboBoxStartType.TabIndex = 2;
            this.comboBoxStartType.SelectedIndexChanged += new System.EventHandler(this.comboBoxStartType_SelectedIndexChanged);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(18, 29);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(62, 13);
            this.label7.TabIndex = 1;
            this.label7.Text = "Start Type:";
            // 
            // buttonChangeAccount
            // 
            this.buttonChangeAccount.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonChangeAccount.Enabled = false;
            this.buttonChangeAccount.Location = new System.Drawing.Point(187, 88);
            this.buttonChangeAccount.Name = "buttonChangeAccount";
            this.buttonChangeAccount.Size = new System.Drawing.Size(245, 21);
            this.buttonChangeAccount.TabIndex = 9;
            this.buttonChangeAccount.Text = "Change Service Settings";
            this.buttonChangeAccount.UseVisualStyleBackColor = true;
            this.buttonChangeAccount.Click += new System.EventHandler(this.buttonChangeAccount_Click);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(18, 109);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(57, 13);
            this.label3.TabIndex = 7;
            this.label3.Text = "Password:";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(18, 83);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(167, 13);
            this.label4.TabIndex = 5;
            this.label4.Text = "User Name  (Computer\\Account):";
            // 
            // textBoxServicePassword
            // 
            this.textBoxServicePassword.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxServicePassword.Location = new System.Drawing.Point(187, 111);
            this.textBoxServicePassword.Name = "textBoxServicePassword";
            this.textBoxServicePassword.Size = new System.Drawing.Size(244, 20);
            this.textBoxServicePassword.TabIndex = 8;
            this.textBoxServicePassword.UseSystemPasswordChar = true;
            this.textBoxServicePassword.TextChanged += new System.EventHandler(this.textBoxServicePassword_TextChanged);
            // 
            // textBoxServiceUserName
            // 
            this.textBoxServiceUserName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxServiceUserName.Location = new System.Drawing.Point(187, 85);
            this.textBoxServiceUserName.Name = "textBoxServiceUserName";
            this.textBoxServiceUserName.Size = new System.Drawing.Size(244, 20);
            this.textBoxServiceUserName.TabIndex = 6;
            this.textBoxServiceUserName.TextChanged += new System.EventHandler(this.textBoxServiceUserName_TextChanged);
            // 
            // comboBoxAccountType
            // 
            this.comboBoxAccountType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.comboBoxAccountType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxAccountType.FormattingEnabled = true;
            this.comboBoxAccountType.Items.AddRange(new object[] {
            "LocalSystem",
            "User"});
            this.comboBoxAccountType.Location = new System.Drawing.Point(187, 58);
            this.comboBoxAccountType.Name = "comboBoxAccountType";
            this.comboBoxAccountType.Size = new System.Drawing.Size(244, 21);
            this.comboBoxAccountType.TabIndex = 4;
            this.comboBoxAccountType.SelectedIndexChanged += new System.EventHandler(this.comboBoxAccountType_SelectedIndexChanged);
            // 
            // buttonSave
            // 
            this.buttonSave.Enabled = false;
            this.buttonSave.Location = new System.Drawing.Point(404, 390);
            this.buttonSave.Name = "buttonSave";
            this.buttonSave.Size = new System.Drawing.Size(103, 23);
            this.buttonSave.TabIndex = 3;
            this.buttonSave.Text = "Save";
            this.buttonSave.UseVisualStyleBackColor = true;
            this.buttonSave.Click += new System.EventHandler(this.buttonSave_Click);
            // 
            // panelDatabaseSetup
            // 
            this.panelDatabaseSetup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelDatabaseSetup.Controls.Add(this.label8);
            this.panelDatabaseSetup.Controls.Add(this.textBoxTargetVersion);
            this.panelDatabaseSetup.Controls.Add(this.label6);
            this.panelDatabaseSetup.Controls.Add(this.label1);
            this.panelDatabaseSetup.Controls.Add(this.textBoxCurrentVersion);
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
            this.panelDatabaseSetup.Location = new System.Drawing.Point(20, 56);
            this.panelDatabaseSetup.Name = "panelDatabaseSetup";
            this.panelDatabaseSetup.Size = new System.Drawing.Size(458, 187);
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
            this.buttonCreateUpdateDatabase.Location = new System.Drawing.Point(187, 139);
            this.buttonCreateUpdateDatabase.Name = "buttonCreateUpdateDatabase";
            this.buttonCreateUpdateDatabase.Size = new System.Drawing.Size(244, 23);
            this.buttonCreateUpdateDatabase.TabIndex = 14;
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
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(30, 116);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(46, 13);
            this.label1.TabIndex = 9;
            this.label1.Text = "Version:";
            // 
            // textBoxCurrentVersion
            // 
            this.textBoxCurrentVersion.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxCurrentVersion.Location = new System.Drawing.Point(236, 112);
            this.textBoxCurrentVersion.Name = "textBoxCurrentVersion";
            this.textBoxCurrentVersion.ReadOnly = true;
            this.textBoxCurrentVersion.Size = new System.Drawing.Size(64, 20);
            this.textBoxCurrentVersion.TabIndex = 11;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(315, 115);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(43, 13);
            this.label6.TabIndex = 12;
            this.label6.Text = "Target:";
            // 
            // textBoxTargetVersion
            // 
            this.textBoxTargetVersion.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxTargetVersion.Location = new System.Drawing.Point(361, 112);
            this.textBoxTargetVersion.Name = "textBoxTargetVersion";
            this.textBoxTargetVersion.ReadOnly = true;
            this.textBoxTargetVersion.Size = new System.Drawing.Size(70, 20);
            this.textBoxTargetVersion.TabIndex = 13;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(185, 115);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(48, 13);
            this.label8.TabIndex = 10;
            this.label8.Text = "Current:";
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
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
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
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox comboBoxStartType;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Button buttonChangeAccount;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox textBoxServicePassword;
        private System.Windows.Forms.TextBox textBoxServiceUserName;
        private System.Windows.Forms.ComboBox comboBoxAccountType;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox textBoxTargetVersion;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBoxCurrentVersion;
    }
}
