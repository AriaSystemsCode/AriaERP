using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Mail;
using System.Text;
using System.Threading.Tasks;

namespace Aria5.DevExpress.MainSystem.WebServiceWorkers
{
    class Email
    {

        string fromEmail     = "Hassan.i@ariasystems.biz";
        string toEmail       = "Hassan.i@ariasystems.biz";
        string emailSubject  = "This is a test mail";
        string emailTitle    = "EmailTitle";
        string emailBody     = "EmailBody";
        string emailPassword = "mbgb_3#4$";
        string emailError = "";

        public string EmailError
        {
            get { return emailError; }
            set { emailError = value; }
        }

        public string EmailPassword
        {
            get { return emailPassword; }
            set { emailPassword = value; }
        }
        string emailHost = "smtp.gmail.com";

        public string EmailHost
        {
            get { return emailHost; }
            set { emailHost = value; }
        }
        int emailPort = 587;

        public int EmailPort
        {
            get { return emailPort; }
            set { emailPort = value; }
        }
        bool emailSSl = true;

        public bool EmailSSl
        {
            get { return emailSSl; }
            set { emailSSl = value; }
        }

        
        public string ToEmail
        {
            get { return toEmail; }
            set { toEmail = value; }
        }

        public string FromEmail
        {
            get { return fromEmail; }
            set { fromEmail = value; }
        }

        public string EmailSubject
        {
            get { return emailSubject; }
            set { emailSubject = value; }
        }

        public string EmailTitle
        {
            get { return emailTitle; }
            set { emailTitle = value; }
        }

        public string EmailBody
        {
            get { return emailBody; }
            set { emailBody = value; }
        }


        public string SendEmail()
        {
            EmailError = "";
            System.Net.Mail.MailMessage mail = new System.Net.Mail.MailMessage();
            mail.To.Add(ToEmail);
            mail.From = new MailAddress(FromEmail, EmailTitle, System.Text.Encoding.UTF8);
            mail.Subject = EmailSubject;
            mail.SubjectEncoding = System.Text.Encoding.UTF8;
            mail.Body = EmailBody;
            mail.BodyEncoding = System.Text.Encoding.UTF8;
            mail.IsBodyHtml = true;
            mail.Priority = MailPriority.High;

            SmtpClient client = new SmtpClient();
            //Add the Creddentials- use your own email id and password

            client.Credentials = new System.Net.NetworkCredential(FromEmail, EmailPassword);

            client.Port = EmailPort; // Gmail works on this port
            client.Host = EmailHost;
            client.EnableSsl = EmailSSl; //Gmail works on Server Secured Layer
            try
            {
                client.Send(mail);
            }
            catch (Exception ex)
            {
                Exception ex2 = ex;
                string errorMessage = string.Empty;
                while (ex2 != null)
                {
                    errorMessage += ex2.ToString();
                    ex2 = ex2.InnerException;
                }
                EmailError = errorMessage;
                //HttpContext.Current.Response.Write(errorMessage );
            } // end try 

            return EmailError;


        }
       
    }
}
