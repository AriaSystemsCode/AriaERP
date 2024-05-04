namespace WindowsFormsApplication1
{
    partial class Form3
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
            this.webservice = new System.Windows.Forms.Button();
            this.frontend = new System.Windows.Forms.Button();
            this.DBVersion = new System.Windows.Forms.Button();
            this.ClientEntity = new System.Windows.Forms.Button();
            this.button1 = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // webservice
            // 
            this.webservice.Location = new System.Drawing.Point(197, 213);
            this.webservice.Name = "webservice";
            this.webservice.Size = new System.Drawing.Size(75, 23);
            this.webservice.TabIndex = 6;
            this.webservice.Text = "web service";
            this.webservice.UseVisualStyleBackColor = true;
            this.webservice.Click += new System.EventHandler(this.webservice_Click);
            // 
            // frontend
            // 
            this.frontend.Location = new System.Drawing.Point(12, 213);
            this.frontend.Name = "frontend";
            this.frontend.Size = new System.Drawing.Size(75, 23);
            this.frontend.TabIndex = 7;
            this.frontend.Text = "Frontend Version";
            this.frontend.UseVisualStyleBackColor = true;
            this.frontend.Click += new System.EventHandler(this.frontend_Click);
            // 
            // DBVersion
            // 
            this.DBVersion.Location = new System.Drawing.Point(105, 213);
            this.DBVersion.Name = "DBVersion";
            this.DBVersion.Size = new System.Drawing.Size(75, 23);
            this.DBVersion.TabIndex = 8;
            this.DBVersion.Text = "DB Version";
            this.DBVersion.UseVisualStyleBackColor = true;
            this.DBVersion.Click += new System.EventHandler(this.DBVersion_Click);
            // 
            // ClientEntity
            // 
            this.ClientEntity.Location = new System.Drawing.Point(12, 12);
            this.ClientEntity.Name = "ClientEntity";
            this.ClientEntity.Size = new System.Drawing.Size(75, 23);
            this.ClientEntity.TabIndex = 9;
            this.ClientEntity.Text = "ClientEntity";
            this.ClientEntity.UseVisualStyleBackColor = true;
            this.ClientEntity.Click += new System.EventHandler(this.ClientEntity_Click);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(82, 81);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(173, 45);
            this.button1.TabIndex = 10;
            this.button1.Text = "Add New Configuration item";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // Form3
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(284, 261);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.ClientEntity);
            this.Controls.Add(this.DBVersion);
            this.Controls.Add(this.frontend);
            this.Controls.Add(this.webservice);
            this.Name = "Form3";
            this.Text = "Form3";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button webservice;
        private System.Windows.Forms.Button frontend;
        private System.Windows.Forms.Button DBVersion;
        private System.Windows.Forms.Button ClientEntity;
        private System.Windows.Forms.Button button1;
    }
}