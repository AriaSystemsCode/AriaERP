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
            this.labelServiceName = new System.Windows.Forms.Label();
            this.labelDisplayName = new System.Windows.Forms.Label();
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
            this.udMaxTime = new System.Windows.Forms.NumericUpDown();
            this.label8 = new System.Windows.Forms.Label();
            this.udMaxRecPerReq = new System.Windows.Forms.NumericUpDown();
            this.label9 = new System.Windows.Forms.Label();
            this.label16 = new System.Windows.Forms.Label();
            this.txtExcNotfEmail = new System.Windows.Forms.TextBox();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderFirewall)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderServiceStop)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotChangeAccount)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDown1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.udMaxTime)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.udMaxRecPerReq)).BeginInit();
            this.SuspendLayout();
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(28, 28);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(265, 13);
            this.label13.TabIndex = 1;
            this.label13.Text = "Use this page to configure the request handler service.";
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
            this.label2.Size = new System.Drawing.Size(77, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Service Name:";
            this.label2.Click += new System.EventHandler(this.label2_Click);
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
            this.label3.Size = new System.Drawing.Size(56, 13);
            this.label3.TabIndex = 18;
            this.label3.Text = "Password:";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(45, 241);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(165, 13);
            this.label4.TabIndex = 16;
            this.label4.Text = "User Name  (Computer\\Account):";
            // 
            // buttonChangeAccount
            // 
            this.buttonChangeAccount.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonChangeAccount.Enabled = false;
            this.buttonChangeAccount.Location = new System.Drawing.Point(238, 294);
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
            this.label6.Size = new System.Drawing.Size(63, 13);
            this.label6.TabIndex = 8;
            this.label6.Text = "Description:";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(45, 187);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(59, 13);
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
            // labelServiceName
            // 
            this.labelServiceName.AutoSize = true;
            this.labelServiceName.Location = new System.Drawing.Point(221, 54);
            this.labelServiceName.Name = "labelServiceName";
            this.labelServiceName.Size = new System.Drawing.Size(102, 13);
            this.labelServiceName.TabIndex = 3;
            this.labelServiceName.Text = "AriaRequestHandler";
            // 
            // labelDisplayName
            // 
            this.labelDisplayName.AutoSize = true;
            this.labelDisplayName.Location = new System.Drawing.Point(221, 84);
            this.labelDisplayName.Name = "labelDisplayName";
            this.labelDisplayName.Size = new System.Drawing.Size(108, 13);
            this.labelDisplayName.TabIndex = 5;
            this.labelDisplayName.Text = "Aria Request Handler";
            // 
            // labelState
            // 
            this.labelState.AutoSize = true;
            this.labelState.Location = new System.Drawing.Point(221, 115);
            this.labelState.Name = "labelState";
            this.labelState.Size = new System.Drawing.Size(37, 13);
            this.labelState.TabIndex = 7;
            this.labelState.Text = "Status";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(45, 115);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(79, 13);
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
            this.label10.Location = new System.Drawing.Point(28, 323);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(220, 13);
            this.label10.TabIndex = 21;
            this.label10.Text = "Max Number Of Records Per Agent Request:";
            this.label10.Click += new System.EventHandler(this.label10_Click);
            // 
            // numericUpDown1
            // 
            this.numericUpDown1.Location = new System.Drawing.Point(256, 320);
            this.numericUpDown1.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.numericUpDown1.Name = "numericUpDown1";
            this.numericUpDown1.Size = new System.Drawing.Size(86, 20);
            this.numericUpDown1.TabIndex = 22;
            this.numericUpDown1.ValueChanged += new System.EventHandler(this.numericUpDown1_ValueChanged);
            // 
            // buttonSave
            // 
            this.buttonSave.Enabled = false;
            this.buttonSave.Location = new System.Drawing.Point(373, 351);
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
            this.label14.Location = new System.Drawing.Point(350, 322);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(105, 13);
            this.label14.TabIndex = 23;
            this.label14.Text = "Set zero for unlimited";
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
            this.label15.Location = new System.Drawing.Point(4, 342);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(348, 26);
            this.label15.TabIndex = 24;
            this.label15.Text = "Notes: Make sure the Request Handler service run on a user which has \r\naccess to " +
                "all Aria folders and the Firewall port 1500 is open.";
            this.label15.Visible = false;
            this.label15.Click += new System.EventHandler(this.label15_Click);
            // 
            // udMaxTime
            // 
            this.udMaxTime.Location = new System.Drawing.Point(256, 344);
            this.udMaxTime.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.udMaxTime.Name = "udMaxTime";
            this.udMaxTime.Size = new System.Drawing.Size(86, 20);
            this.udMaxTime.TabIndex = 26;
            this.udMaxTime.ValueChanged += new System.EventHandler(this.udMaxTime_ValueChanged);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(28, 347);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(168, 13);
            this.label8.TabIndex = 25;
            this.label8.Text = "Max Execution Time Per Request:";
            // 
            // udMaxRecPerReq
            // 
            this.udMaxRecPerReq.Location = new System.Drawing.Point(256, 368);
            this.udMaxRecPerReq.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.udMaxRecPerReq.Name = "udMaxRecPerReq";
            this.udMaxRecPerReq.Size = new System.Drawing.Size(86, 20);
            this.udMaxRecPerReq.TabIndex = 28;
            this.udMaxRecPerReq.ValueChanged += new System.EventHandler(this.udMaxRecPerReq_ValueChanged);
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(28, 370);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(181, 13);
            this.label9.TabIndex = 27;
            this.label9.Text = "Max Number Of Records Per Report:";
            // 
            // label16
            // 
            this.label16.AutoSize = true;
            this.label16.Location = new System.Drawing.Point(28, 393);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(154, 13);
            this.label16.TabIndex = 29;
            this.label16.Text = "Exceed Limit Notification Email:";
            // 
            // txtExcNotfEmail
            // 
            this.txtExcNotfEmail.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.txtExcNotfEmail.Location = new System.Drawing.Point(256, 393);
            this.txtExcNotfEmail.Name = "txtExcNotfEmail";
            this.txtExcNotfEmail.Size = new System.Drawing.Size(212, 20);
            this.txtExcNotfEmail.TabIndex = 30;
            this.txtExcNotfEmail.TextChanged += new System.EventHandler(this.txtExcNotfEmail_TextChanged);
            // 
            // RequestHandlerServiceControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.txtExcNotfEmail);
            this.Controls.Add(this.label16);
            this.Controls.Add(this.udMaxRecPerReq);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.udMaxTime);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.label15);
            this.Controls.Add(this.label14);
            this.Controls.Add(this.buttonSave);
            this.Controls.Add(this.numericUpDown1);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.buttonRun);
            this.Controls.Add(this.buttonEnableFirewall);
            this.Controls.Add(this.labelState);
            this.Controls.Add(this.label11);
            this.Controls.Add(this.labelDisplayName);
            this.Controls.Add(this.labelServiceName);
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
            this.Size = new System.Drawing.Size(516, 421);
            this.Load += new System.EventHandler(this.RequestHandlerServiceControl_Load);
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderFirewall)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderServiceStop)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderCannotChangeAccount)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDown1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.udMaxTime)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.udMaxRecPerReq)).EndInit();
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
        private System.Windows.Forms.Label labelServiceName;
        private System.Windows.Forms.Label labelDisplayName;
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
        private System.Windows.Forms.NumericUpDown udMaxRecPerReq;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.NumericUpDown udMaxTime;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox txtExcNotfEmail;
        private System.Windows.Forms.Label label16;
    }
}
