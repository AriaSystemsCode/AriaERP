namespace FixOldSaasClinets
{
    partial class Fix
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Fix));
            this.cmdfixxml = new System.Windows.Forms.Button();
            this.txtsqlserver = new System.Windows.Forms.TextBox();
            this.lblsqlserver = new System.Windows.Forms.Label();
            this.lblusername = new System.Windows.Forms.Label();
            this.txtusername = new System.Windows.Forms.TextBox();
            this.chkwindowsauth = new System.Windows.Forms.CheckBox();
            this.lblpassword = new System.Windows.Forms.Label();
            this.txtpassword = new System.Windows.Forms.TextBox();
            this.lblProgress = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // cmdfixxml
            // 
            this.cmdfixxml.Enabled = false;
            this.cmdfixxml.Location = new System.Drawing.Point(142, 110);
            this.cmdfixxml.Name = "cmdfixxml";
            this.cmdfixxml.Size = new System.Drawing.Size(115, 23);
            this.cmdfixxml.TabIndex = 4;
            this.cmdfixxml.Text = "Fix Client Data";
            this.cmdfixxml.UseVisualStyleBackColor = true;
            this.cmdfixxml.Click += new System.EventHandler(this.cmdfixxml_Click);
            // 
            // txtsqlserver
            // 
            this.txtsqlserver.Location = new System.Drawing.Point(75, 11);
            this.txtsqlserver.Name = "txtsqlserver";
            this.txtsqlserver.Size = new System.Drawing.Size(201, 20);
            this.txtsqlserver.TabIndex = 0;
            this.txtsqlserver.TextChanged += new System.EventHandler(this.txtsqlserver_TextChanged);
            // 
            // lblsqlserver
            // 
            this.lblsqlserver.AutoSize = true;
            this.lblsqlserver.Location = new System.Drawing.Point(4, 15);
            this.lblsqlserver.Name = "lblsqlserver";
            this.lblsqlserver.Size = new System.Drawing.Size(68, 13);
            this.lblsqlserver.TabIndex = 2;
            this.lblsqlserver.Text = "SQL Server :";
            // 
            // lblusername
            // 
            this.lblusername.AutoSize = true;
            this.lblusername.Location = new System.Drawing.Point(9, 43);
            this.lblusername.Name = "lblusername";
            this.lblusername.Size = new System.Drawing.Size(63, 13);
            this.lblusername.TabIndex = 4;
            this.lblusername.Text = "User Name:";
            // 
            // txtusername
            // 
            this.txtusername.Location = new System.Drawing.Point(75, 39);
            this.txtusername.Name = "txtusername";
            this.txtusername.Size = new System.Drawing.Size(201, 20);
            this.txtusername.TabIndex = 2;
            this.txtusername.TextChanged += new System.EventHandler(this.txtusername_TextChanged);
            // 
            // chkwindowsauth
            // 
            this.chkwindowsauth.AutoSize = true;
            this.chkwindowsauth.Location = new System.Drawing.Point(298, 13);
            this.chkwindowsauth.Name = "chkwindowsauth";
            this.chkwindowsauth.Size = new System.Drawing.Size(98, 17);
            this.chkwindowsauth.TabIndex = 1;
            this.chkwindowsauth.Text = "Windows Auth.";
            this.chkwindowsauth.UseVisualStyleBackColor = true;
            this.chkwindowsauth.CheckedChanged += new System.EventHandler(this.chkwindowsauth_CheckedChanged);
            // 
            // lblpassword
            // 
            this.lblpassword.AutoSize = true;
            this.lblpassword.Location = new System.Drawing.Point(13, 69);
            this.lblpassword.Name = "lblpassword";
            this.lblpassword.Size = new System.Drawing.Size(59, 13);
            this.lblpassword.TabIndex = 7;
            this.lblpassword.Text = "Password :";
            // 
            // txtpassword
            // 
            this.txtpassword.Location = new System.Drawing.Point(75, 65);
            this.txtpassword.Name = "txtpassword";
            this.txtpassword.PasswordChar = '*';
            this.txtpassword.Size = new System.Drawing.Size(201, 20);
            this.txtpassword.TabIndex = 3;
            this.txtpassword.TextChanged += new System.EventHandler(this.txtpassword_TextChanged);
            // 
            // lblProgress
            // 
            this.lblProgress.AutoSize = true;
            this.lblProgress.Location = new System.Drawing.Point(122, 151);
            this.lblProgress.Name = "lblProgress";
            this.lblProgress.Size = new System.Drawing.Size(0, 13);
            this.lblProgress.TabIndex = 7;
            // 
            // Fix
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(398, 176);
            this.Controls.Add(this.lblProgress);
            this.Controls.Add(this.lblpassword);
            this.Controls.Add(this.txtpassword);
            this.Controls.Add(this.chkwindowsauth);
            this.Controls.Add(this.lblusername);
            this.Controls.Add(this.txtusername);
            this.Controls.Add(this.lblsqlserver);
            this.Controls.Add(this.txtsqlserver);
            this.Controls.Add(this.cmdfixxml);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "Fix";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Fix SAAS Client(s) ENV. By RB [SAAS Version] Changes";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button cmdfixxml;
        private System.Windows.Forms.TextBox txtsqlserver;
        private System.Windows.Forms.Label lblsqlserver;
        private System.Windows.Forms.Label lblusername;
        private System.Windows.Forms.TextBox txtusername;
        private System.Windows.Forms.CheckBox chkwindowsauth;
        private System.Windows.Forms.Label lblpassword;
        private System.Windows.Forms.TextBox txtpassword;
        private System.Windows.Forms.Label lblProgress;
    }
}