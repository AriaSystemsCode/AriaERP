﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Mail;
using System.Text;
using System.Threading.Tasks;

namespace Aria5SystemAdmin.Module.Entities
{

   public class Email
    {

        string fromEmail     = "Hassan.i@ariasystems.biz";
        string toEmail       = "Hassan.i@ariasystems.biz";
        string emailSubject  = "This is a test mail";
        string emailTitle    = "EmailTitle";
        string emailBody     = "EmailBody";
        string emailPassword = "mbgb_3#4$";
        string emailError    = "";
        int    emailPort     = 587;
        string emailHost     = "smtp.gmail.com";
        bool   emailSSL      = true;


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
        
        public string EmailHost
        {
            get { return emailHost; }
            set { emailHost = value; }
        }

        public int EmailPort
        {
            get { return emailPort; }
            set { emailPort = value; }
        }

        public bool EmailSSL
        {
            get { return emailSSL; }
            set { emailSSL = value; }
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

       //Methods
        public string SendEmail()
        {
            EmailError = "";

            #region Validation
            if (string.IsNullOrEmpty(FromEmail) ) EmailError = "From Email is empty.";
            if (string.IsNullOrEmpty(ToEmail       )) EmailError = "From Email is empty.";
            if (string.IsNullOrEmpty(EmailSubject  )) EmailError = "Email Subject is empty.";
            if (string.IsNullOrEmpty(EmailTitle    )) EmailError = "Email Title is empty.";
            if (string.IsNullOrEmpty(EmailBody     )) EmailError = "Email Body is empty.";
            if (string.IsNullOrEmpty(EmailPassword )) EmailError = "Email Password is empty.";
            if (EmailPort<0)                          EmailError = "Email Port is less than 0.";
            if (string.IsNullOrEmpty(EmailHost     )) EmailError = "Email Host is empty.";

            #endregion Validation

            #region send email
            if (string.IsNullOrEmpty(EmailError) == true)
            {
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
                client.EnableSsl = EmailSSL; //Gmail works on Server Secured Layer
                try
                {
                    client.Send(mail);
                }
                catch (Exception exception)
                {
                    Exception exception2 = exception;
                    string errorMessage = string.Empty;
                    while (exception2 != null)
                    {
                        errorMessage += exception2.ToString();
                        exception2 = exception2.InnerException;
                    }
                    EmailError = errorMessage;
                    //HttpContext.Current.Response.Write(errorMessage );
                } // end try 
            }
            #endregion send email

            return EmailError;
        }
    }
}