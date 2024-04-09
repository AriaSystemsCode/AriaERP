namespace Aria.Configuration.Server.Controls
{
    partial class MutliDatabaseServer
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
            this.button1 = new System.Windows.Forms.Button();
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
            this.labelLoginType = new System.Windows.Forms.Label();
            this.labelPassword = new System.Windows.Forms.Label();
            this.labelUserName = new System.Windows.Forms.Label();
            this.textBoxPassword = new System.Windows.Forms.TextBox();
            this.textBoxUserName = new System.Windows.Forms.TextBox();
            this.comboBoxLoginType = new System.Windows.Forms.ComboBox();
            this.labelServerName = new System.Windows.Forms.Label();
            this.comboBoxServerName = new System.Windows.Forms.ComboBox();
            this.errorProviderCannotCaonnect = new System.Windows.Forms.ErrorProvider(this.components);
            this.folderBrowserDialogAria4XPSharedPath = new System.Windows.Forms.FolderBrowserDialog();
            this.errorProviderInvalidFolder = new System.Windows.Forms.ErrorProvider(this.components);
            this.panelMain.SuspendLayout();
            this.panel1.SuspendLayout();
            this.panelDatabaseSetup.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotCaonnect)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderInvalidFolder)).BeginInit();
            this.SuspendLayout();
            // 
            // labelDatabaseSetup
            // 
            this.labelDatabaseSetup.AutoSize = true;
            this.labelDatabaseSetup.BackColor = System.Drawing.Color.Transparent;
            this.labelDatabaseSetup.Font = new System.Drawing.Font("Tahoma", 12.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelDatabaseSetup.Location = new System.Drawing.Point(5, 10);
            this.labelDatabaseSetup.Name = "labelDatabaseSetup";
            this.labelDatabaseSetup.Size = new System.Drawing.Size(266, 21);
            this.labelDatabaseSetup.TabIndex = 3;
            this.labelDatabaseSetup.Text = "Database Configuration (x86)";
            // 
            // labelNotes
            // 
            this.labelNotes.AutoSize = true;
            this.labelNotes.Location = new System.Drawing.Point(29, 36);
            this.labelNotes.Name = "labelNotes";
            this.labelNotes.Size = new System.Drawing.Size(432, 26);
            this.labelNotes.TabIndex = 4;
            this.labelNotes.Text = "Specify the sql server will be used to handle the comunication between applicatio" +
                "n (x86) \r\nand the sql server (x64).";
            // 
            // panelMain
            // 
            this.panelMain.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelMain.Controls.Add(this.button1);
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
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(8, 393);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(215, 23);
            this.button1.TabIndex = 26;
            this.button1.Text = "Install SQL Server Express (x86) ...";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
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
            this.panel1.Location = new System.Drawing.Point(8, 178);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(500, 199);
            this.panel1.TabIndex = 7;
            this.panel1.Visible = false;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.BackColor = System.Drawing.Color.Transparent;
            this.label5.Font = new System.Drawing.Font("Tahoma", 12.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(3, 0);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(285, 21);
            this.label5.TabIndex = 0;
            this.label5.Text = "Database Service Account (x86)";
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
            this.label15.Location = new System.Drawing.Point(4, 170);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(418, 26);
            this.label15.TabIndex = 25;
            this.label15.Text = "Notes: Make sure the SQL Server Windows service run on a user which has access to" +
                " \r\nall Aria folders.";
            this.label15.Visible = false;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(18, 65);
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
            this.comboBoxStartType.Location = new System.Drawing.Point(196, 40);
            this.comboBoxStartType.Name = "comboBoxStartType";
            this.comboBoxStartType.Size = new System.Drawing.Size(244, 21);
            this.comboBoxStartType.TabIndex = 2;
            this.comboBoxStartType.SelectedIndexChanged += new System.EventHandler(this.comboBoxStartType_SelectedIndexChanged);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(18, 38);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(62, 13);
            this.label7.TabIndex = 1;
            this.label7.Text = "Start Type:";
            // 
            // buttonChangeAccount
            // 
            this.buttonChangeAccount.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonChangeAccount.Enabled = false;
            this.buttonChangeAccount.Location = new System.Drawing.Point(196, 146);
            this.buttonChangeAccount.Name = "buttonChangeAccount";
            this.buttonChangeAccount.Size = new System.Drawing.Size(244, 21);
            this.buttonChangeAccount.TabIndex = 9;
            this.buttonChangeAccount.Text = "Change Service Settings";
            this.buttonChangeAccount.UseVisualStyleBackColor = true;
            this.buttonChangeAccount.Click += new System.EventHandler(this.buttonChangeAccount_Click);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(18, 118);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(57, 13);
            this.label3.TabIndex = 7;
            this.label3.Text = "Password:";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(18, 92);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(167, 13);
            this.label4.TabIndex = 5;
            this.label4.Text = "User Name  (Computer\\Account):";
            // 
            // textBoxServicePassword
            // 
            this.textBoxServicePassword.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxServicePassword.Location = new System.Drawing.Point(196, 120);
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
            this.textBoxServiceUserName.Location = new System.Drawing.Point(196, 94);
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
            this.comboBoxAccountType.Location = new System.Drawing.Point(196, 67);
            this.comboBoxAccountType.Name = "comboBoxAccountType";
            this.comboBoxAccountType.Size = new System.Drawing.Size(244, 21);
            this.comboBoxAccountType.TabIndex = 4;
            this.comboBoxAccountType.SelectedIndexChanged += new System.EventHandler(this.comboBoxAccountType_SelectedIndexChanged);
            // 
            // buttonSave
            // 
            this.buttonSave.Enabled = false;
            this.buttonSave.Location = new System.Drawing.Point(381, 393);
            this.buttonSave.Name = "buttonSave";
            this.buttonSave.Size = new System.Drawing.Size(103, 23);
            this.buttonSave.TabIndex = 8;
            this.buttonSave.Text = "Save";
            this.buttonSave.UseVisualStyleBackColor = true;
            this.buttonSave.Click += new System.EventHandler(this.buttonSave_Click);
            // 
            // panelDatabaseSetup
            // 
            this.panelDatabaseSetup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelDatabaseSetup.Controls.Add(this.buttonTestConection);
            this.panelDatabaseSetup.Controls.Add(this.labelLoginType);
            this.panelDatabaseSetup.Controls.Add(this.labelPassword);
            this.panelDatabaseSetup.Controls.Add(this.labelUserName);
            this.panelDatabaseSetup.Controls.Add(this.textBoxPassword);
            this.panelDatabaseSetup.Controls.Add(this.textBoxUserName);
            this.panelDatabaseSetup.Controls.Add(this.comboBoxLoginType);
            this.panelDatabaseSetup.Controls.Add(this.labelServerName);
            this.panelDatabaseSetup.Controls.Add(this.comboBoxServerName);
            this.panelDatabaseSetup.Location = new System.Drawing.Point(29, 79);
            this.panelDatabaseSetup.Name = "panelDatabaseSetup";
            this.panelDatabaseSetup.Size = new System.Drawing.Size(458, 120);
            this.panelDatabaseSetup.TabIndex = 6;
            // 
            // buttonTestConection
            // 
            this.buttonTestConection.Location = new System.Drawing.Point(154, 87);
            this.buttonTestConection.Name = "buttonTestConection";
            this.buttonTestConection.Size = new System.Drawing.Size(274, 23);
            this.buttonTestConection.TabIndex = 8;
            this.buttonTestConection.Text = "Test Connection";
            this.buttonTestConection.UseVisualStyleBackColor = true;
            this.buttonTestConection.Click += new System.EventHandler(this.buttonTestConection_Click);
            // 
            // labelLoginType
            // 
            this.labelLoginType.AutoSize = true;
            this.labelLoginType.Location = new System.Drawing.Point(27, 41);
            this.labelLoginType.Name = "labelLoginType";
            this.labelLoginType.Size = new System.Drawing.Size(63, 13);
            this.labelLoginType.TabIndex = 2;
            this.labelLoginType.Text = "Login Type:";
            // 
            // labelPassword
            // 
            this.labelPassword.AutoSize = true;
            this.labelPassword.Location = new System.Drawing.Point(273, 65);
            this.labelPassword.Name = "labelPassword";
            this.labelPassword.Size = new System.Drawing.Size(57, 13);
            this.labelPassword.TabIndex = 6;
            this.labelPassword.Text = "Password:";
            // 
            // labelUserName
            // 
            this.labelUserName.AutoSize = true;
            this.labelUserName.Location = new System.Drawing.Point(27, 66);
            this.labelUserName.Name = "labelUserName";
            this.labelUserName.Size = new System.Drawing.Size(63, 13);
            this.labelUserName.TabIndex = 4;
            this.labelUserName.Text = "User Name:";
            // 
            // textBoxPassword
            // 
            this.textBoxPassword.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxPassword.Location = new System.Drawing.Point(336, 61);
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
            this.textBoxUserName.Location = new System.Drawing.Point(154, 62);
            this.textBoxUserName.Name = "textBoxUserName";
            this.textBoxUserName.Size = new System.Drawing.Size(113, 20);
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
            this.comboBoxLoginType.Location = new System.Drawing.Point(154, 35);
            this.comboBoxLoginType.Name = "comboBoxLoginType";
            this.comboBoxLoginType.Size = new System.Drawing.Size(275, 21);
            this.comboBoxLoginType.TabIndex = 3;
            this.comboBoxLoginType.SelectedIndexChanged += new System.EventHandler(this.comboBoxLoginType_SelectedIndexChanged);
            // 
            // labelServerName
            // 
            this.labelServerName.AutoSize = true;
            this.labelServerName.Location = new System.Drawing.Point(27, 14);
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
            this.comboBoxServerName.Location = new System.Drawing.Point(154, 8);
            this.comboBoxServerName.Name = "comboBoxServerName";
            this.comboBoxServerName.Size = new System.Drawing.Size(275, 21);
            this.comboBoxServerName.TabIndex = 1;
            this.comboBoxServerName.SelectedIndexChanged += new System.EventHandler(this.comboBoxServerName_SelectedIndexChanged);
            this.comboBoxServerName.DropDown += new System.EventHandler(this.comboBoxServerName_DropDown);
            // 
            // errorProviderCannotCaonnect
            // 
            this.errorProviderCannotCaonnect.ContainerControl = this;
            // 
            // errorProviderInvalidFolder
            // 
            this.errorProviderInvalidFolder.ContainerControl = this;
            // 
            // MutliDatabaseServer
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.panelMain);
            this.Name = "MutliDatabaseServer";
            this.Size = new System.Drawing.Size(516, 425);
            this.panelMain.ResumeLayout(false);
            this.panelMain.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.panelDatabaseSetup.ResumeLayout(false);
            this.panelDatabaseSetup.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotCaonnect)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderInvalidFolder)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label labelDatabaseSetup;
        private System.Windows.Forms.Label labelNotes;
        private System.Windows.Forms.Panel panelMain;
        private System.Windows.Forms.ComboBox comboBoxServerName;
        private System.Windows.Forms.Label labelServerName;
        private System.Windows.Forms.Panel panelDatabaseSetup;
        private System.Windows.Forms.Label labelLoginType;
        private System.Windows.Forms.Label labelPassword;
        private System.Windows.Forms.Label labelUserName;
        private System.Windows.Forms.TextBox textBoxPassword;
        private System.Windows.Forms.TextBox textBoxUserName;
        private System.Windows.Forms.ComboBox comboBoxLoginType;
        private System.Windows.Forms.ErrorProvider errorProviderCannotCaonnect;
        private System.Windows.Forms.Button buttonTestConection;
        private System.Windows.Forms.Button buttonSave;
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialogAria4XPSharedPath;
        private System.Windows.Forms.ErrorProvider errorProviderInvalidFolder;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox comboBoxStartType;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Button buttonChangeAccount;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox textBoxServicePassword;
        private System.Windows.Forms.TextBox textBoxServiceUserName;
        private System.Windows.Forms.ComboBox comboBoxAccountType;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.Button button1;
    }
}
