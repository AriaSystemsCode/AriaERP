namespace Aria.Configuration.Server.Controls
{
    partial class RequestHandlerServiceControl
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
            this.label13 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.comboBoxAccountType = new System.Windows.Forms.ComboBox();
            this.textBoxPassword = new System.Windows.Forms.TextBox();
            this.textBoxUserName = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.buttonChangeAccount = new System.Windows.Forms.Button();
            this.label5 = new System.Windows.Forms.Label();
            this.textBoxDescription = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.comboBoxStartType = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.errorProviderFirewall = new System.Windows.Forms.ErrorProvider(this.components);
            this.errorProviderServiceStop = new System.Windows.Forms.ErrorProvider(this.components);
            this.label8 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.labelState = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.buttonEnableFirewall = new System.Windows.Forms.Button();
            this.buttonRun = new System.Windows.Forms.Button();
            this.timerStatus = new System.Windows.Forms.Timer(this.components);
            this.errorProviderCannotChangeAccount = new System.Windows.Forms.ErrorProvider(this.components);
            this.label10 = new System.Windows.Forms.Label();
            this.numericUpDown1 = new System.Windows.Forms.NumericUpDown();
            this.buttonSave = new System.Windows.Forms.Button();
            this.label14 = new System.Windows.Forms.Label();
            this.label15 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderFirewall)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderServiceStop)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotChangeAccount)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDown1)).BeginInit();
            this.SuspendLayout();
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(28, 28);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(316, 13);
            this.label13.TabIndex = 1;
            this.label13.Text = "Use this page to configure the request handler windows service.";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.BackColor = System.Drawing.Color.Transparent;
            this.label12.Font = new System.Drawing.Font("Tahoma", 12.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label12.Location = new System.Drawing.Point(3, 0);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(295, 21);
            this.label12.TabIndex = 0;
            this.label12.Text = "Request Handler Service Settings";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(45, 54);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(76, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Service Name:";
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
            this.comboBoxAccountType.Location = new System.Drawing.Point(238, 216);
            this.comboBoxAccountType.Name = "comboBoxAccountType";
            this.comboBoxAccountType.Size = new System.Drawing.Size(230, 21);
            this.comboBoxAccountType.TabIndex = 15;
            this.comboBoxAccountType.SelectedIndexChanged += new System.EventHandler(this.comboBoxAccountType_SelectedIndexChanged);
            // 
            // textBoxPassword
            // 
            this.textBoxPassword.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxPassword.Location = new System.Drawing.Point(238, 269);
            this.textBoxPassword.Name = "textBoxPassword";
            this.textBoxPassword.Size = new System.Drawing.Size(230, 20);
            this.textBoxPassword.TabIndex = 19;
            this.textBoxPassword.UseSystemPasswordChar = true;
            this.textBoxPassword.TextChanged += new System.EventHandler(this.textBoxPassword_TextChanged);
            // 
            // textBoxUserName
            // 
            this.textBoxUserName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxUserName.Location = new System.Drawing.Point(238, 243);
            this.textBoxUserName.Name = "textBoxUserName";
            this.textBoxUserName.Size = new System.Drawing.Size(230, 20);
            this.textBoxUserName.TabIndex = 17;
            this.textBoxUserName.TextChanged += new System.EventHandler(this.textBoxUserName_TextChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(45, 267);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(57, 13);
            this.label3.TabIndex = 18;
            this.label3.Text = "Password:";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(45, 241);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(167, 13);
            this.label4.TabIndex = 16;
            this.label4.Text = "User Name  (Computer\\Account):";
            // 
            // buttonChangeAccount
            // 
            this.buttonChangeAccount.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonChangeAccount.Enabled = false;
            this.buttonChangeAccount.Location = new System.Drawing.Point(238, 311);
            this.buttonChangeAccount.Name = "buttonChangeAccount";
            this.buttonChangeAccount.Size = new System.Drawing.Size(230, 21);
            this.buttonChangeAccount.TabIndex = 20;
            this.buttonChangeAccount.Text = "Change Service Settings";
            this.buttonChangeAccount.UseVisualStyleBackColor = true;
            this.buttonChangeAccount.Click += new System.EventHandler(this.buttonChangeAccount_Click);
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(45, 84);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(75, 13);
            this.label5.TabIndex = 4;
            this.label5.Text = "Display Name:";
            // 
            // textBoxDescription
            // 
            this.textBoxDescription.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxDescription.Location = new System.Drawing.Point(48, 155);
            this.textBoxDescription.Multiline = true;
            this.textBoxDescription.Name = "textBoxDescription";
            this.textBoxDescription.ReadOnly = true;
            this.textBoxDescription.Size = new System.Drawing.Size(420, 26);
            this.textBoxDescription.TabIndex = 9;
            this.textBoxDescription.Text = "Schedule Aria Request Handler Recurring Tasks";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(45, 139);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(64, 13);
            this.label6.TabIndex = 8;
            this.label6.Text = "Description:";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(45, 187);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(62, 13);
            this.label7.TabIndex = 12;
            this.label7.Text = "Start Type:";
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
            this.comboBoxStartType.Location = new System.Drawing.Point(238, 189);
            this.comboBoxStartType.Name = "comboBoxStartType";
            this.comboBoxStartType.Size = new System.Drawing.Size(230, 21);
            this.comboBoxStartType.TabIndex = 13;
            this.comboBoxStartType.SelectedIndexChanged += new System.EventHandler(this.comboBoxStartType_SelectedIndexChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(45, 214);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(77, 13);
            this.label1.TabIndex = 14;
            this.label1.Text = "Account Type:";
            // 
            // errorProviderFirewall
            // 
            this.errorProviderFirewall.ContainerControl = this;
            // 
            // errorProviderServiceStop
            // 
            this.errorProviderServiceStop.ContainerControl = this;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(221, 59);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(109, 13);
            this.label8.TabIndex = 3;
            this.label8.Text = "Aria Request Handler";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(221, 89);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(109, 13);
            this.label9.TabIndex = 5;
            this.label9.Text = "Aria Request Handler";
            // 
            // labelState
            // 
            this.labelState.AutoSize = true;
            this.labelState.Location = new System.Drawing.Point(221, 119);
            this.labelState.Name = "labelState";
            this.labelState.Size = new System.Drawing.Size(38, 13);
            this.labelState.TabIndex = 7;
            this.labelState.Text = "Status";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(45, 115);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(80, 13);
            this.label11.TabIndex = 6;
            this.label11.Text = "Service Status:";
            // 
            // buttonEnableFirewall
            // 
            this.buttonEnableFirewall.Location = new System.Drawing.Point(372, 54);
            this.buttonEnableFirewall.Name = "buttonEnableFirewall";
            this.buttonEnableFirewall.Size = new System.Drawing.Size(96, 23);
            this.buttonEnableFirewall.TabIndex = 10;
            this.buttonEnableFirewall.Text = "Enable Firewall";
            this.buttonEnableFirewall.UseVisualStyleBackColor = true;
            this.buttonEnableFirewall.Visible = false;
            this.buttonEnableFirewall.Click += new System.EventHandler(this.buttonEnableFirewall_Click);
            // 
            // buttonRun
            // 
            this.buttonRun.Location = new System.Drawing.Point(372, 114);
            this.buttonRun.Name = "buttonRun";
            this.buttonRun.Size = new System.Drawing.Size(96, 23);
            this.buttonRun.TabIndex = 11;
            this.buttonRun.Text = "Start Service";
            this.buttonRun.UseVisualStyleBackColor = true;
            this.buttonRun.Click += new System.EventHandler(this.buttonRun_Click);
            // 
            // timerStatus
            // 
            this.timerStatus.Enabled = true;
            this.timerStatus.Interval = 1000;
            // 
            // errorProviderCannotChangeAccount
            // 
            this.errorProviderCannotChangeAccount.ContainerControl = this;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(45, 331);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(181, 13);
            this.label10.TabIndex = 21;
            this.label10.Text = "Max number of records per request:";
            // 
            // numericUpDown1
            // 
            this.numericUpDown1.Location = new System.Drawing.Point(238, 329);
            this.numericUpDown1.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.numericUpDown1.Name = "numericUpDown1";
            this.numericUpDown1.Size = new System.Drawing.Size(106, 20);
            this.numericUpDown1.TabIndex = 22;
            this.numericUpDown1.ValueChanged += new System.EventHandler(this.numericUpDown1_ValueChanged);
            // 
            // buttonSave
            // 
            this.buttonSave.Enabled = false;
            this.buttonSave.Location = new System.Drawing.Point(372, 368);
            this.buttonSave.Name = "buttonSave";
            this.buttonSave.Size = new System.Drawing.Size(96, 23);
            this.buttonSave.TabIndex = 24;
            this.buttonSave.Text = "Save";
            this.buttonSave.UseVisualStyleBackColor = true;
            this.buttonSave.Click += new System.EventHandler(this.buttonSave_Click);
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(350, 331);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(109, 13);
            this.label14.TabIndex = 23;
            this.label14.Text = "Set zero for unlimited";
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
            this.label15.Location = new System.Drawing.Point(4, 352);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(350, 26);
            this.label15.TabIndex = 24;
            this.label15.Text = "Notes: Make sure the Request Handler service run on a user which has \r\naccess to " +
                "all Aria folders and the Firewall port 1500 is open.";
            this.label15.Click += new System.EventHandler(this.label15_Click);
            // 
            // RequestHandlerServiceControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.label15);
            this.Controls.Add(this.label14);
            this.Controls.Add(this.buttonSave);
            this.Controls.Add(this.numericUpDown1);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.buttonRun);
            this.Controls.Add(this.buttonEnableFirewall);
            this.Controls.Add(this.labelState);
            this.Controls.Add(this.label11);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.comboBoxStartType);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.textBoxDescription);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.buttonChangeAccount);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.textBoxPassword);
            this.Controls.Add(this.textBoxUserName);
            this.Controls.Add(this.comboBoxAccountType);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.label12);
            this.Name = "RequestHandlerServiceControl";
            this.Size = new System.Drawing.Size(516, 409);
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderFirewall)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderServiceStop)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotChangeAccount)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDown1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox comboBoxAccountType;
        private System.Windows.Forms.TextBox textBoxPassword;
        private System.Windows.Forms.TextBox textBoxUserName;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Button buttonChangeAccount;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox textBoxDescription;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.ComboBox comboBoxStartType;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ErrorProvider errorProviderFirewall;
        private System.Windows.Forms.ErrorProvider errorProviderServiceStop;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label labelState;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.Button buttonEnableFirewall;
        private System.Windows.Forms.Button buttonRun;
        private System.Windows.Forms.Timer timerStatus;
        private System.Windows.Forms.ErrorProvider errorProviderCannotChangeAccount;
        private System.Windows.Forms.Button buttonSave;
        private System.Windows.Forms.NumericUpDown numericUpDown1;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.Label label15;
    }
}
