namespace net.authorize.sample
{
    partial class FormChargeCustomerProfile
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
            this.textBoxAmount = new System.Windows.Forms.TextBox();
            this.label12 = new System.Windows.Forms.Label();
            this.buttonPay = new System.Windows.Forms.Button();
            this.textBoxTransactionId = new System.Windows.Forms.TextBox();
            this.label13 = new System.Windows.Forms.Label();
            this.textBoxCustomerProfile = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.checkBoxAuthorizeOnly = new System.Windows.Forms.CheckBox();
            this.textBoxPaymentProfileID = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // textBoxAmount
            // 
            this.textBoxAmount.Location = new System.Drawing.Point(140, 94);
            this.textBoxAmount.Name = "textBoxAmount";
            this.textBoxAmount.Size = new System.Drawing.Size(60, 20);
            this.textBoxAmount.TabIndex = 25;
            this.textBoxAmount.Text = "15.00";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(12, 94);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(48, 13);
            this.label12.TabIndex = 22;
            this.label12.Text = "Amount:";
            // 
            // buttonPay
            // 
            this.buttonPay.Location = new System.Drawing.Point(420, 137);
            this.buttonPay.Name = "buttonPay";
            this.buttonPay.Size = new System.Drawing.Size(75, 23);
            this.buttonPay.TabIndex = 19;
            this.buttonPay.Text = "Pay";
            this.buttonPay.UseVisualStyleBackColor = true;
            this.buttonPay.Click += new System.EventHandler(this.buttonPay_Click);
            // 
            // textBoxTransactionId
            // 
            this.textBoxTransactionId.Location = new System.Drawing.Point(140, 139);
            this.textBoxTransactionId.Name = "textBoxTransactionId";
            this.textBoxTransactionId.ReadOnly = true;
            this.textBoxTransactionId.Size = new System.Drawing.Size(204, 20);
            this.textBoxTransactionId.TabIndex = 7;
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(10, 139);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(80, 13);
            this.label13.TabIndex = 6;
            this.label13.Text = "Transaction Id:";
            // 
            // textBoxCustomerProfile
            // 
            this.textBoxCustomerProfile.Location = new System.Drawing.Point(140, 18);
            this.textBoxCustomerProfile.Name = "textBoxCustomerProfile";
            this.textBoxCustomerProfile.Size = new System.Drawing.Size(204, 20);
            this.textBoxCustomerProfile.TabIndex = 21;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(10, 18);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(55, 13);
            this.label1.TabIndex = 20;
            this.label1.Text = "Profile ID:";
            // 
            // checkBoxAuthorizeOnly
            // 
            this.checkBoxAuthorizeOnly.AutoSize = true;
            this.checkBoxAuthorizeOnly.Location = new System.Drawing.Point(219, 94);
            this.checkBoxAuthorizeOnly.Name = "checkBoxAuthorizeOnly";
            this.checkBoxAuthorizeOnly.Size = new System.Drawing.Size(97, 17);
            this.checkBoxAuthorizeOnly.TabIndex = 26;
            this.checkBoxAuthorizeOnly.Text = "Authorize Only";
            this.checkBoxAuthorizeOnly.UseVisualStyleBackColor = true;
            // 
            // textBoxPaymentProfileID
            // 
            this.textBoxPaymentProfileID.Location = new System.Drawing.Point(140, 44);
            this.textBoxPaymentProfileID.Name = "textBoxPaymentProfileID";
            this.textBoxPaymentProfileID.Size = new System.Drawing.Size(204, 20);
            this.textBoxPaymentProfileID.TabIndex = 28;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(10, 44);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(100, 13);
            this.label2.TabIndex = 27;
            this.label2.Text = "Payment Profile ID:";
            // 
            // FormChargeCustomerProfile
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(523, 171);
            this.Controls.Add(this.textBoxPaymentProfileID);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.checkBoxAuthorizeOnly);
            this.Controls.Add(this.textBoxAmount);
            this.Controls.Add(this.textBoxCustomerProfile);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.textBoxTransactionId);
            this.Controls.Add(this.label12);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.buttonPay);
            this.Name = "FormChargeCustomerProfile";
            this.Text = "Charge Customer Profile:";
            this.Load += new System.EventHandler(this.FormChargeCustomerProfile_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.TextBox textBoxAmount;
        private System.Windows.Forms.Button buttonPay;
        private System.Windows.Forms.TextBox textBoxTransactionId;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.TextBox textBoxCustomerProfile;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox checkBoxAuthorizeOnly;
        private System.Windows.Forms.TextBox textBoxPaymentProfileID;
        private System.Windows.Forms.Label label2;
    }
}