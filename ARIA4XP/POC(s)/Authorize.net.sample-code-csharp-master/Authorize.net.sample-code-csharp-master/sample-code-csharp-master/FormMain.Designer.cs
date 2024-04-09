namespace net.authorize.sample
{
    partial class FormMain
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
            this.buttonChargeCreditCard = new System.Windows.Forms.Button();
            this.buttonAuthorize = new System.Windows.Forms.Button();
            this.buttonCapture = new System.Windows.Forms.Button();
            this.buttonVoid = new System.Windows.Forms.Button();
            this.button1 = new System.Windows.Forms.Button();
            this.buttonCharegeCustomerProfile = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // buttonChargeCreditCard
            // 
            this.buttonChargeCreditCard.Location = new System.Drawing.Point(28, 13);
            this.buttonChargeCreditCard.Name = "buttonChargeCreditCard";
            this.buttonChargeCreditCard.Size = new System.Drawing.Size(244, 23);
            this.buttonChargeCreditCard.TabIndex = 0;
            this.buttonChargeCreditCard.Text = "Charege Credit Card";
            this.buttonChargeCreditCard.UseVisualStyleBackColor = true;
            this.buttonChargeCreditCard.Click += new System.EventHandler(this.buttonChargeCreditCard_Click);
            // 
            // buttonAuthorize
            // 
            this.buttonAuthorize.Location = new System.Drawing.Point(28, 42);
            this.buttonAuthorize.Name = "buttonAuthorize";
            this.buttonAuthorize.Size = new System.Drawing.Size(244, 23);
            this.buttonAuthorize.TabIndex = 1;
            this.buttonAuthorize.Text = "Authorize Credit Card";
            this.buttonAuthorize.UseVisualStyleBackColor = true;
            this.buttonAuthorize.Click += new System.EventHandler(this.buttonAuthorize_Click);
            // 
            // buttonCapture
            // 
            this.buttonCapture.Location = new System.Drawing.Point(28, 71);
            this.buttonCapture.Name = "buttonCapture";
            this.buttonCapture.Size = new System.Drawing.Size(244, 23);
            this.buttonCapture.TabIndex = 2;
            this.buttonCapture.Text = "Capture";
            this.buttonCapture.UseVisualStyleBackColor = true;
            this.buttonCapture.Click += new System.EventHandler(this.buttonCapture_Click);
            // 
            // buttonVoid
            // 
            this.buttonVoid.Location = new System.Drawing.Point(28, 100);
            this.buttonVoid.Name = "buttonVoid";
            this.buttonVoid.Size = new System.Drawing.Size(244, 23);
            this.buttonVoid.TabIndex = 3;
            this.buttonVoid.Text = "Void";
            this.buttonVoid.UseVisualStyleBackColor = true;
            this.buttonVoid.Click += new System.EventHandler(this.buttonVoid_Click);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(28, 133);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(244, 23);
            this.button1.TabIndex = 4;
            this.button1.Text = "Create Customer Profile";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // buttonCharegeCustomerProfile
            // 
            this.buttonCharegeCustomerProfile.Location = new System.Drawing.Point(28, 162);
            this.buttonCharegeCustomerProfile.Name = "buttonCharegeCustomerProfile";
            this.buttonCharegeCustomerProfile.Size = new System.Drawing.Size(244, 23);
            this.buttonCharegeCustomerProfile.TabIndex = 5;
            this.buttonCharegeCustomerProfile.Text = "Charge Customer Profile";
            this.buttonCharegeCustomerProfile.UseVisualStyleBackColor = true;
            this.buttonCharegeCustomerProfile.Click += new System.EventHandler(this.buttonCharegeCustomerProfile_Click);
            // 
            // FormMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(284, 261);
            this.Controls.Add(this.buttonCharegeCustomerProfile);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.buttonVoid);
            this.Controls.Add(this.buttonCapture);
            this.Controls.Add(this.buttonAuthorize);
            this.Controls.Add(this.buttonChargeCreditCard);
            this.Name = "FormMain";
            this.Text = "FormMain";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button buttonChargeCreditCard;
        private System.Windows.Forms.Button buttonAuthorize;
        private System.Windows.Forms.Button buttonCapture;
        private System.Windows.Forms.Button buttonVoid;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button buttonCharegeCustomerProfile;
    }
}