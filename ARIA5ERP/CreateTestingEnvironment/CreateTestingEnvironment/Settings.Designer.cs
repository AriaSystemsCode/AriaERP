namespace CreateTestingEnvironment
{
    partial class Settings
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.lblSelect = new System.Windows.Forms.Label();
            this.txtPath = new System.Windows.Forms.TextBox();
            this.btnSelect = new System.Windows.Forms.Button();
            this.folderBrowserDialog1 = new System.Windows.Forms.FolderBrowserDialog();
            this.lblServers = new System.Windows.Forms.Label();
            this.CmbServerName = new System.Windows.Forms.ComboBox();
            this.lblUser = new System.Windows.Forms.Label();
            this.txtUserName = new System.Windows.Forms.TextBox();
            this.txtPassword = new System.Windows.Forms.TextBox();
            this.lblPw = new System.Windows.Forms.Label();
            this.btnTestConn = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnSave = new System.Windows.Forms.Button();
            this.btnSqlBackup = new System.Windows.Forms.Button();
            this.txtSQLBackup = new System.Windows.Forms.TextBox();
            this.lblSqlBackup = new System.Windows.Forms.Label();
            this.btnSqlDestPath = new System.Windows.Forms.Button();
            this.txtSQLDstPath = new System.Windows.Forms.TextBox();
            this.lblDBPath = new System.Windows.Forms.Label();
            this.txtLivePW = new System.Windows.Forms.TextBox();
            this.lblLvPassword = new System.Windows.Forms.Label();
            this.txtLiveUser = new System.Windows.Forms.TextBox();
            this.lblLvUser = new System.Windows.Forms.Label();
            this.cmbLvServer = new System.Windows.Forms.ComboBox();
            this.lbllvserver = new System.Windows.Forms.Label();
            this.btnLvConnection = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // lblSelect
            // 
            this.lblSelect.AutoSize = true;
            this.lblSelect.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblSelect.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblSelect.Location = new System.Drawing.Point(6, 5);
            this.lblSelect.Name = "lblSelect";
            this.lblSelect.Size = new System.Drawing.Size(112, 15);
            this.lblSelect.TabIndex = 1;
            this.lblSelect.Text = "Destination Folder:";
            // 
            // txtPath
            // 
            this.txtPath.Location = new System.Drawing.Point(6, 24);
            this.txtPath.Name = "txtPath";
            this.txtPath.ReadOnly = true;
            this.txtPath.Size = new System.Drawing.Size(460, 20);
            this.txtPath.TabIndex = 2;
            // 
            // btnSelect
            // 
            this.btnSelect.Location = new System.Drawing.Point(474, 22);
            this.btnSelect.Name = "btnSelect";
            this.btnSelect.Size = new System.Drawing.Size(29, 23);
            this.btnSelect.TabIndex = 3;
            this.btnSelect.Text = "...";
            this.btnSelect.UseVisualStyleBackColor = true;
            this.btnSelect.Click += new System.EventHandler(this.button1_Click);
            // 
            // lblServers
            // 
            this.lblServers.AutoSize = true;
            this.lblServers.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblServers.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblServers.Location = new System.Drawing.Point(6, 140);
            this.lblServers.Name = "lblServers";
            this.lblServers.Size = new System.Drawing.Size(118, 15);
            this.lblServers.TabIndex = 4;
            this.lblServers.Text = "Testing SQL server:";
            // 
            // CmbServerName
            // 
            this.CmbServerName.FormattingEnabled = true;
            this.CmbServerName.Location = new System.Drawing.Point(6, 158);
            this.CmbServerName.Name = "CmbServerName";
            this.CmbServerName.Size = new System.Drawing.Size(244, 21);
            this.CmbServerName.TabIndex = 5;
            // 
            // lblUser
            // 
            this.lblUser.AutoSize = true;
            this.lblUser.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblUser.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblUser.Location = new System.Drawing.Point(6, 188);
            this.lblUser.Name = "lblUser";
            this.lblUser.Size = new System.Drawing.Size(183, 15);
            this.lblUser.TabIndex = 6;
            this.lblUser.Text = "Testing SQL server user Name:";
            // 
            // txtUserName
            // 
            this.txtUserName.Location = new System.Drawing.Point(6, 206);
            this.txtUserName.Name = "txtUserName";
            this.txtUserName.Size = new System.Drawing.Size(193, 20);
            this.txtUserName.TabIndex = 7;
            // 
            // txtPassword
            // 
            this.txtPassword.Location = new System.Drawing.Point(6, 253);
            this.txtPassword.Name = "txtPassword";
            this.txtPassword.Size = new System.Drawing.Size(193, 20);
            this.txtPassword.TabIndex = 9;
            this.txtPassword.UseSystemPasswordChar = true;
            // 
            // lblPw
            // 
            this.lblPw.AutoSize = true;
            this.lblPw.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblPw.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblPw.Location = new System.Drawing.Point(6, 235);
            this.lblPw.Name = "lblPw";
            this.lblPw.Size = new System.Drawing.Size(179, 15);
            this.lblPw.TabIndex = 8;
            this.lblPw.Text = "Testing SQL server Password:";
            // 
            // btnTestConn
            // 
            this.btnTestConn.Location = new System.Drawing.Point(7, 279);
            this.btnTestConn.Name = "btnTestConn";
            this.btnTestConn.Size = new System.Drawing.Size(106, 23);
            this.btnTestConn.TabIndex = 10;
            this.btnTestConn.Text = "Test Connection";
            this.btnTestConn.UseVisualStyleBackColor = true;
            this.btnTestConn.Click += new System.EventHandler(this.btnTestConn_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.Location = new System.Drawing.Point(263, 311);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(75, 23);
            this.btnCancel.TabIndex = 11;
            this.btnCancel.Text = "&Cancel";
            this.btnCancel.UseVisualStyleBackColor = true;
            this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
            // 
            // btnSave
            // 
            this.btnSave.Location = new System.Drawing.Point(169, 311);
            this.btnSave.Name = "btnSave";
            this.btnSave.Size = new System.Drawing.Size(75, 23);
            this.btnSave.TabIndex = 12;
            this.btnSave.Text = "&Save";
            this.btnSave.UseVisualStyleBackColor = true;
            this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
            // 
            // btnSqlBackup
            // 
            this.btnSqlBackup.Location = new System.Drawing.Point(473, 68);
            this.btnSqlBackup.Name = "btnSqlBackup";
            this.btnSqlBackup.Size = new System.Drawing.Size(30, 23);
            this.btnSqlBackup.TabIndex = 15;
            this.btnSqlBackup.Text = "...";
            this.btnSqlBackup.UseVisualStyleBackColor = true;
            this.btnSqlBackup.Click += new System.EventHandler(this.btnSqlBackup_Click);
            // 
            // txtSQLBackup
            // 
            this.txtSQLBackup.Location = new System.Drawing.Point(5, 70);
            this.txtSQLBackup.Name = "txtSQLBackup";
            this.txtSQLBackup.ReadOnly = true;
            this.txtSQLBackup.Size = new System.Drawing.Size(460, 20);
            this.txtSQLBackup.TabIndex = 14;
            // 
            // lblSqlBackup
            // 
            this.lblSqlBackup.AutoSize = true;
            this.lblSqlBackup.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblSqlBackup.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblSqlBackup.Location = new System.Drawing.Point(5, 51);
            this.lblSqlBackup.Name = "lblSqlBackup";
            this.lblSqlBackup.Size = new System.Drawing.Size(139, 15);
            this.lblSqlBackup.TabIndex = 13;
            this.lblSqlBackup.Text = "Database Backup Path:";
            // 
            // btnSqlDestPath
            // 
            this.btnSqlDestPath.Location = new System.Drawing.Point(473, 113);
            this.btnSqlDestPath.Name = "btnSqlDestPath";
            this.btnSqlDestPath.Size = new System.Drawing.Size(30, 23);
            this.btnSqlDestPath.TabIndex = 18;
            this.btnSqlDestPath.Text = "...";
            this.btnSqlDestPath.UseVisualStyleBackColor = true;
            this.btnSqlDestPath.Click += new System.EventHandler(this.btnSqlDestPath_Click);
            // 
            // txtSQLDstPath
            // 
            this.txtSQLDstPath.Location = new System.Drawing.Point(5, 115);
            this.txtSQLDstPath.Name = "txtSQLDstPath";
            this.txtSQLDstPath.ReadOnly = true;
            this.txtSQLDstPath.Size = new System.Drawing.Size(460, 20);
            this.txtSQLDstPath.TabIndex = 17;
            // 
            // lblDBPath
            // 
            this.lblDBPath.AutoSize = true;
            this.lblDBPath.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblDBPath.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblDBPath.Location = new System.Drawing.Point(5, 96);
            this.lblDBPath.Name = "lblDBPath";
            this.lblDBPath.Size = new System.Drawing.Size(120, 15);
            this.lblDBPath.TabIndex = 16;
            this.lblDBPath.Text = "Test Database Path:";
            // 
            // txtLivePW
            // 
            this.txtLivePW.Location = new System.Drawing.Point(259, 253);
            this.txtLivePW.Name = "txtLivePW";
            this.txtLivePW.Size = new System.Drawing.Size(193, 20);
            this.txtLivePW.TabIndex = 24;
            this.txtLivePW.UseSystemPasswordChar = true;
            // 
            // lblLvPassword
            // 
            this.lblLvPassword.AutoSize = true;
            this.lblLvPassword.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblLvPassword.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblLvPassword.Location = new System.Drawing.Point(259, 235);
            this.lblLvPassword.Name = "lblLvPassword";
            this.lblLvPassword.Size = new System.Drawing.Size(161, 15);
            this.lblLvPassword.TabIndex = 23;
            this.lblLvPassword.Text = "Live SQL server Password:";
            // 
            // txtLiveUser
            // 
            this.txtLiveUser.Location = new System.Drawing.Point(259, 206);
            this.txtLiveUser.Name = "txtLiveUser";
            this.txtLiveUser.Size = new System.Drawing.Size(193, 20);
            this.txtLiveUser.TabIndex = 22;
            // 
            // lblLvUser
            // 
            this.lblLvUser.AutoSize = true;
            this.lblLvUser.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblLvUser.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblLvUser.Location = new System.Drawing.Point(259, 188);
            this.lblLvUser.Name = "lblLvUser";
            this.lblLvUser.Size = new System.Drawing.Size(165, 15);
            this.lblLvUser.TabIndex = 21;
            this.lblLvUser.Text = "Live SQL server user Name:";
            // 
            // cmbLvServer
            // 
            this.cmbLvServer.FormattingEnabled = true;
            this.cmbLvServer.Location = new System.Drawing.Point(259, 158);
            this.cmbLvServer.Name = "cmbLvServer";
            this.cmbLvServer.Size = new System.Drawing.Size(244, 21);
            this.cmbLvServer.TabIndex = 20;
            // 
            // lbllvserver
            // 
            this.lbllvserver.AutoSize = true;
            this.lbllvserver.Font = new System.Drawing.Font("Arial", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lbllvserver.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lbllvserver.Location = new System.Drawing.Point(259, 140);
            this.lbllvserver.Name = "lbllvserver";
            this.lbllvserver.Size = new System.Drawing.Size(234, 15);
            this.lbllvserver.TabIndex = 19;
            this.lbllvserver.Text = "Live SQL server:(System.Master server)";
            // 
            // btnLvConnection
            // 
            this.btnLvConnection.Location = new System.Drawing.Point(259, 279);
            this.btnLvConnection.Name = "btnLvConnection";
            this.btnLvConnection.Size = new System.Drawing.Size(106, 23);
            this.btnLvConnection.TabIndex = 25;
            this.btnLvConnection.Text = "Test Connection";
            this.btnLvConnection.UseVisualStyleBackColor = true;
            this.btnLvConnection.Click += new System.EventHandler(this.button1_Click_1);
            // 
            // Settings
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Info;
            this.ClientSize = new System.Drawing.Size(507, 345);
            this.Controls.Add(this.btnLvConnection);
            this.Controls.Add(this.txtLivePW);
            this.Controls.Add(this.lblLvPassword);
            this.Controls.Add(this.txtLiveUser);
            this.Controls.Add(this.lblLvUser);
            this.Controls.Add(this.cmbLvServer);
            this.Controls.Add(this.lbllvserver);
            this.Controls.Add(this.btnSqlDestPath);
            this.Controls.Add(this.txtSQLDstPath);
            this.Controls.Add(this.lblDBPath);
            this.Controls.Add(this.btnSqlBackup);
            this.Controls.Add(this.txtSQLBackup);
            this.Controls.Add(this.lblSqlBackup);
            this.Controls.Add(this.btnSave);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.btnTestConn);
            this.Controls.Add(this.txtPassword);
            this.Controls.Add(this.lblPw);
            this.Controls.Add(this.txtUserName);
            this.Controls.Add(this.lblUser);
            this.Controls.Add(this.CmbServerName);
            this.Controls.Add(this.lblServers);
            this.Controls.Add(this.btnSelect);
            this.Controls.Add(this.txtPath);
            this.Controls.Add(this.lblSelect);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Name = "Settings";
            this.ShowIcon = false;
            this.Text = "Settings";
            this.TopMost = true;
            this.Load += new System.EventHandler(this.Settings_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label lblSelect;
        private System.Windows.Forms.TextBox txtPath;
        private System.Windows.Forms.Button btnSelect;
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog1;
        private System.Windows.Forms.Label lblServers;
        private System.Windows.Forms.ComboBox CmbServerName;
        private System.Windows.Forms.Label lblUser;
        private System.Windows.Forms.TextBox txtUserName;
        private System.Windows.Forms.TextBox txtPassword;
        private System.Windows.Forms.Label lblPw;
        private System.Windows.Forms.Button btnTestConn;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnSave;
        private System.Windows.Forms.Button btnSqlBackup;
        private System.Windows.Forms.TextBox txtSQLBackup;
        private System.Windows.Forms.Label lblSqlBackup;
        private System.Windows.Forms.Button btnSqlDestPath;
        private System.Windows.Forms.TextBox txtSQLDstPath;
        private System.Windows.Forms.Label lblDBPath;
        private System.Windows.Forms.TextBox txtLivePW;
        private System.Windows.Forms.Label lblLvPassword;
        private System.Windows.Forms.TextBox txtLiveUser;
        private System.Windows.Forms.Label lblLvUser;
        private System.Windows.Forms.ComboBox cmbLvServer;
        private System.Windows.Forms.Label lbllvserver;
        private System.Windows.Forms.Button btnLvConnection;
    }
}