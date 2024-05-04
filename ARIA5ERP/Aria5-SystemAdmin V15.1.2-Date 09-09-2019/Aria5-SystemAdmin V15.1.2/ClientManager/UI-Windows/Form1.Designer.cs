namespace UI_Windows
{
    partial class Form1
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
            this.btnStart = new System.Windows.Forms.Button();
            this.txtClientCode = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.txtSharedPath = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.txtSqlDBPath = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.txtAriaSourcePath = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.txtAriaMasterSourcePath = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.txtServerName = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.txtSqlServerDB = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.txtSqlUserName = new System.Windows.Forms.TextBox();
            this.label9 = new System.Windows.Forms.Label();
            this.txtSqlPassword = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.button1 = new System.Windows.Forms.Button();
            this.txtADSearch = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // btnStart
            // 
            this.btnStart.Location = new System.Drawing.Point(173, 413);
            this.btnStart.Name = "btnStart";
            this.btnStart.Size = new System.Drawing.Size(148, 46);
            this.btnStart.TabIndex = 0;
            this.btnStart.Text = "Start";
            this.btnStart.UseVisualStyleBackColor = true;
            this.btnStart.Click += new System.EventHandler(this.btnStart_Click);
            // 
            // txtClientCode
            // 
            this.txtClientCode.Location = new System.Drawing.Point(163, 16);
            this.txtClientCode.Name = "txtClientCode";
            this.txtClientCode.Size = new System.Drawing.Size(249, 20);
            this.txtClientCode.TabIndex = 1;
            this.txtClientCode.Text = "Test1";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(29, 19);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(62, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = "Client Code";
            // 
            // txtSharedPath
            // 
            this.txtSharedPath.Location = new System.Drawing.Point(163, 58);
            this.txtSharedPath.Name = "txtSharedPath";
            this.txtSharedPath.Size = new System.Drawing.Size(249, 20);
            this.txtSharedPath.TabIndex = 1;
            this.txtSharedPath.Text = "D:\\CMTest\\Shared";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(29, 61);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(66, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Shared Path";
            // 
            // txtSqlDBPath
            // 
            this.txtSqlDBPath.Location = new System.Drawing.Point(163, 94);
            this.txtSqlDBPath.Name = "txtSqlDBPath";
            this.txtSqlDBPath.Size = new System.Drawing.Size(249, 20);
            this.txtSqlDBPath.TabIndex = 1;
            this.txtSqlDBPath.Text = "D:\\CMTest\\SQL";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(29, 97);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(62, 13);
            this.label4.TabIndex = 2;
            this.label4.Text = "Sql DB Path";
            // 
            // txtAriaSourcePath
            // 
            this.txtAriaSourcePath.Location = new System.Drawing.Point(163, 142);
            this.txtAriaSourcePath.Name = "txtAriaSourcePath";
            this.txtAriaSourcePath.Size = new System.Drawing.Size(249, 20);
            this.txtAriaSourcePath.TabIndex = 1;
            this.txtAriaSourcePath.Text = "D:\\CMTest\\Aria Source";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(29, 145);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(87, 13);
            this.label5.TabIndex = 2;
            this.label5.Text = "Aria Source Path";
            // 
            // txtAriaMasterSourcePath
            // 
            this.txtAriaMasterSourcePath.Location = new System.Drawing.Point(163, 185);
            this.txtAriaMasterSourcePath.Name = "txtAriaMasterSourcePath";
            this.txtAriaMasterSourcePath.Size = new System.Drawing.Size(249, 20);
            this.txtAriaMasterSourcePath.TabIndex = 1;
            this.txtAriaMasterSourcePath.Text = "D:\\CMTest\\Aria Master Path";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(29, 188);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(120, 13);
            this.label6.TabIndex = 2;
            this.label6.Text = "AriaMaster Source Path";
            // 
            // txtServerName
            // 
            this.txtServerName.Location = new System.Drawing.Point(163, 243);
            this.txtServerName.Name = "txtServerName";
            this.txtServerName.Size = new System.Drawing.Size(249, 20);
            this.txtServerName.TabIndex = 1;
            this.txtServerName.Text = ".";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(29, 246);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(85, 13);
            this.label7.TabIndex = 2;
            this.label7.Text = "Sql Server name";
            // 
            // txtSqlServerDB
            // 
            this.txtSqlServerDB.Location = new System.Drawing.Point(163, 281);
            this.txtSqlServerDB.Name = "txtSqlServerDB";
            this.txtSqlServerDB.Size = new System.Drawing.Size(249, 20);
            this.txtSqlServerDB.TabIndex = 1;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(29, 284);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(72, 13);
            this.label8.TabIndex = 2;
            this.label8.Text = "Sql Server DB";
            // 
            // txtSqlUserName
            // 
            this.txtSqlUserName.Location = new System.Drawing.Point(163, 319);
            this.txtSqlUserName.Name = "txtSqlUserName";
            this.txtSqlUserName.Size = new System.Drawing.Size(249, 20);
            this.txtSqlUserName.TabIndex = 1;
            this.txtSqlUserName.Text = "CM";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(29, 322);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(108, 13);
            this.label9.TabIndex = 2;
            this.label9.Text = "Sql Server UserName";
            // 
            // txtSqlPassword
            // 
            this.txtSqlPassword.Location = new System.Drawing.Point(163, 359);
            this.txtSqlPassword.Name = "txtSqlPassword";
            this.txtSqlPassword.Size = new System.Drawing.Size(249, 20);
            this.txtSqlPassword.TabIndex = 1;
            this.txtSqlPassword.Text = "CM";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(29, 362);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(105, 13);
            this.label10.TabIndex = 2;
            this.label10.Text = "Sql Server Password";
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(375, 413);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(127, 46);
            this.button1.TabIndex = 3;
            this.button1.Text = "button1";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // txtADSearch
            // 
            this.txtADSearch.Location = new System.Drawing.Point(173, 387);
            this.txtADSearch.Name = "txtADSearch";
            this.txtADSearch.Size = new System.Drawing.Size(249, 20);
            this.txtADSearch.TabIndex = 1;
            this.txtADSearch.Text = "D:\\CMTest\\Aria Master Path";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(514, 472);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.txtSqlPassword);
            this.Controls.Add(this.txtSqlUserName);
            this.Controls.Add(this.txtSqlServerDB);
            this.Controls.Add(this.txtServerName);
            this.Controls.Add(this.txtADSearch);
            this.Controls.Add(this.txtAriaMasterSourcePath);
            this.Controls.Add(this.txtAriaSourcePath);
            this.Controls.Add(this.txtSqlDBPath);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.txtSharedPath);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.txtClientCode);
            this.Controls.Add(this.btnStart);
            this.Name = "Form1";
            this.Text = "Form1";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button btnStart;
        private System.Windows.Forms.TextBox txtClientCode;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox txtSharedPath;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox txtSqlDBPath;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox txtAriaSourcePath;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox txtAriaMasterSourcePath;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox txtServerName;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox txtSqlServerDB;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox txtSqlUserName;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.TextBox txtSqlPassword;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.TextBox txtADSearch;
    }
}

