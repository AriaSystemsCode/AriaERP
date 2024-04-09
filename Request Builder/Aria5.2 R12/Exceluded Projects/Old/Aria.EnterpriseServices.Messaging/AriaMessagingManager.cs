using System.Text;
using Aria.DataTypes.Messaging;
using System.Net;
using System.Net.Mail;
using Aria.Environment;
using Aria.Data;
using Aria.DataTypes.ObjectDictionary;
using Aria.Utilities.ParameterSubstitution;
using System.Collections;
using Aria.Xml;
using System.Diagnostics;
using System;
using System.IO;

namespace Aria.EnterpriseServices.Messaging
{
    /// <summary>
    /// This class used to send email by varies parameter and varies type. 
    /// </summary>
    public class AriaMessagingManager : MarshalByRefObject
    {
        private AriaEnviromentVariables _enviromentVariables = new AriaEnviromentVariables();


        private MailMessage GetMailMessage(AriaDbConnection connection, AriaArgumentList argumentList, AriaEmail message, string clientId)
        {
            
            AriaParameterSubstituter parameterSubstituter = new AriaParameterSubstituter(connection, argumentList,clientId );
            
            MailMessage mailMessage = new MailMessage();

            //T20100512.0026 Hassan 2010 05 23 [Begin]
            _enviromentVariables.ClientID = clientId;
            _enviromentVariables.ConnectionsRefresh();
            //T20100512.0026 Hassan 2010 05 23 [END]

            mailMessage.From = new MailAddress(_enviromentVariables.AriaSenderMail, _enviromentVariables.AriaSenderName, Encoding.UTF8);

            if (message.To != null && message.To.Length > 0)
            {
                string[] emailTo = message.To.Trim().Split(',');

                for (int index = 0; index < emailTo.Length; index++)
                {
                    string mailAdd = parameterSubstituter.GetSubstitutedText(emailTo[index],clientId ).Trim();
                    if (mailAdd.Equals(""))
                        continue;
                    //[ADD]Ahmed Maher -Date: 09/04/2010 -Error for Group Notify
                    if (mailAdd.Contains(","))
                    {
                        string[] emailCcSubstituter = mailAdd.Trim().Split(',');
                        for (int i = 0; i < emailCcSubstituter.Length; i++)
                        {
                            mailMessage.To.Add(new MailAddress(emailCcSubstituter[i]));
                        }
                    }
                    else
                        mailMessage.To.Add(new MailAddress(mailAdd));
                    //[END]
                }
            }

            if (message.Cc != null && message.Cc.Length > 0)
            {
                string[] emailCc = message.Cc.Trim().Split(',');

                for (int index = 0; index < emailCc.Length; index++)
                {
                    string mailAdd = parameterSubstituter.GetSubstitutedText(emailCc[index],clientId ).Trim();
                    if (mailAdd.Equals(""))
                        continue;
                    //[ADD]Ahmed Maher -Date: 09/04/2010 -Error for Group Notify
                    if (mailAdd.Contains(","))
                    {
                        string[] emailCcSubstituter = mailAdd.Trim().Split(',');
                        for (int i = 0; i < emailCcSubstituter.Length; i++)
                        {
                            mailMessage.CC.Add(new MailAddress(emailCcSubstituter[i]));
                        }
                    }
                    else
                        mailMessage.CC.Add(new MailAddress(mailAdd));
                    //[END]
                }
            }

            if (message.Bcc != null && message.Bcc.Length > 0)
            {
                string[] emailBcc = message.Bcc.Trim().Split(',');

                for (int index = 0; index < emailBcc.Length; index++)
                {
                    string mailAdd = parameterSubstituter.GetSubstitutedText(emailBcc[index],clientId ).Trim();
                    if (mailAdd.Equals(""))
                        continue;
                    //[ADD]Ahmed Maher -Date: 09/04/2010 -Error for Group Notify
                    if (mailAdd.Contains(","))
                    {
                        string[] emailCcSubstituter = mailAdd.Trim().Split(',');
                        for (int i = 0; i < emailCcSubstituter.Length; i++)
                        {
                            mailMessage.Bcc.Add(new MailAddress(emailCcSubstituter[i]));
                        }
                    }
                    else
                        mailMessage.Bcc.Add(new MailAddress(mailAdd));
                    //[END]
                }
            }

            if (message.Subject != null)
            {
                mailMessage.Subject = parameterSubstituter.GetSubstitutedText(message.Subject,clientId );

                mailMessage.SubjectEncoding = Encoding.UTF8;
            }

            if (message.Body != null)
            {
                mailMessage.Body = parameterSubstituter.GetSubstitutedText(message.Body,clientId).Replace("\n", "<br>");


                mailMessage.BodyEncoding = Encoding.UTF8;
            }
            
    
            foreach (DictionaryEntry entry in message.Attachment)
            {
                //mailMessage.Attachments.Add(new Attachment(parameterSubstituter.GetTimeStamp() + " - " + parameterSubstituter.GetSubstitutedText(entry.Key.ToString())));
                mailMessage.Attachments.Add(new Attachment(parameterSubstituter.GetSubstitutedText(entry.Key.ToString(),clientId )));

                if (entry.Value.ToString().Trim() != "" && entry.Value != null)
                    mailMessage.Attachments[mailMessage.Attachments.Count - 1].Name = entry.Value.ToString();
            }

            mailMessage.IsBodyHtml = true;

            mailMessage.Priority = MailPriority.High;

            return mailMessage;
        }

        public void SendEmail(AriaDbConnection connection, AriaArgumentList argumentList, AriaEmail mailMessage, string clientId)
        {
           
            SmtpClient smtp = new SmtpClient();
            //T20100512.0026 Hassan 2010 05 23 [Begin]
            _enviromentVariables.ClientID = clientId;
            _enviromentVariables.ConnectionsRefresh();
            //T20100512.0026 Hassan 2010 05 23 [END]


            // T20110803.0001 MAH JULY 7 2011
            ////Mostafa 10-10-2010 task T20101004.0035 Start
            //_enviromentVariables.GetAria27CompanyDataConnectionString(connection.CompanyName);
            ////Mostafa 10-10-2010 task T20101004.0035 End
            _enviromentVariables.CompanyID = connection.CompanyName;
            // T20110803.0001 MAH JULY 7 2011End

			
            smtp.Port = _enviromentVariables.AriaSMTPPort;

            smtp.Host = _enviromentVariables.AriaSMTPHost;

            //[START]Ahmed Maher -Date: 2-12-2009 -SSL Fix
            //smtp.EnableSsl = true;
            // T20110803.0001 MAH JULY 7 2011
            //smtp.EnableSsl = _enviromentVariables.Ssl;
            smtp.EnableSsl = _enviromentVariables.AriaSsl;
            // T20110803.0001 MAH JULY 7 2011 End
            //[END]

            smtp.Credentials = new NetworkCredential(_enviromentVariables.AriaSMTPUserName, _enviromentVariables.AriaSMTPPassword);

            //try
            {
                MailMessage convertedMessage = GetMailMessage(connection, argumentList, mailMessage,clientId);

                if (!(convertedMessage.To.Count == 0 && convertedMessage.CC.Count == 0 && convertedMessage.Bcc.Count == 0))
                {
                    smtp.Send(convertedMessage);
                }
            }
            //catch (Exception e)
            //{
            //    EventLog error = new EventLog();
            //    error.Log = "Application";
            //    error.Source = "AriaSendEmail";
            //    error.WriteEntry("Mail Cannot be sent\\" + e.Message);
            //    error.Close();
            //}
        }

        public void SendEmail(AriaEmail email, string clientId)
        {
            MailMessage mailMessage = new MailMessage();

            //T20100512.0026 Hassan 2010 05 23 [Begin]
            _enviromentVariables.ClientID = clientId;
            _enviromentVariables.ConnectionsRefresh();
            //T20100512.0026 Hassan 2010 05 23 [END]


            mailMessage.From = new MailAddress(_enviromentVariables.AriaSenderMail, _enviromentVariables.AriaSenderName, Encoding.UTF8);

            mailMessage.To.Add(new MailAddress(email.To, _enviromentVariables.AriaSenderName, Encoding.UTF8));

            mailMessage.Subject = email.Subject;

            mailMessage.Body = email.Body;

            SmtpClient smtp = new SmtpClient();

            smtp.Port = _enviromentVariables.AriaSMTPPort;

            smtp.Host = _enviromentVariables.AriaSMTPHost;
            
            //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [Start]
            //smtp.EnableSsl = true;
            // T20110803.0001 MAH JULY 7 2011
            //smtp.EnableSsl = _enviromentVariables.Ssl;
            smtp.EnableSsl = _enviromentVariables.AriaSsl;
            // T20110803.0001 MAH JULY 7 2011 End
            //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [End]

            smtp.Credentials = new NetworkCredential(_enviromentVariables.AriaSMTPUserName, _enviromentVariables.AriaSMTPPassword);

            smtp.Send(mailMessage);
        }

        //[Add]Ahmed Maher -Date:17/03/2010
        private SmtpClient _smtp = null;

        public SmtpClient smtp
        {
            get
            {
                if (_smtp == null)
                    _smtp = new SmtpClient();
                return _smtp;
            }
        }

        private MailMessage _mailMessage = null;

        public MailMessage mailMessage
        {
            get
            {
                if (_mailMessage == null)
                    _mailMessage = new MailMessage();
                return _mailMessage;
            }
        }

        private string _emailBody = "";
        /// <summary>
        /// Get/Set body of send email.
        /// </summary>
        public string EmailBody
        {
            get
            {
                return _emailBody;
            }
            set
            {
                _emailBody = value;
            }
        }

        private bool _isBodyHtmal = false;

        /// <summary>
        /// Get/Set if body Support HTML Tabg or not.
        /// </summary>
        public bool IsBodyHtmal
        {
            get { return _isBodyHtmal; }
            set { _isBodyHtmal = value; }
        }

        private string _emailSubject = "";
        /// <summary>
        /// This property used to get or set Subject of Email.
        /// </summary>
        public string EmailSubject
        {
            get { return _emailSubject; }
            set { _emailSubject = value; }
        }

        private bool _error = false;

        /// <summary>
        /// Used to Check if sending email generate error or not <c>(Read only)</c>.
        /// </summary>
        public bool Error
        {
            get { return _error; }
        }

        private string _errorMessage = "";
        /// <summary>
        /// Used to Get message error if sending email generate error <c>(Read only)</c>.
        /// </summary>
        public string ErrorMessage
        {
            get { return _errorMessage; }
        }

        private string _sMTPServer = "";
        /// <summary>
        /// Get or Set SMTP Server.
        /// </summary>
        public string SMTPServer
        {
            get { return _sMTPServer; }
            set { _sMTPServer = value; }
        }
        private int _sMTPPort = 0;
        /// <summary>
        /// Get or Set SMTP Port.
        /// </summary>
        public int SMTPPort
        {
            get { return _sMTPPort; }
            set { _sMTPPort = value; }
        }
        private string _sMTPUsername = "";
        /// <summary>
        /// Get or Set Email that enable us from send email
        /// </summary>
        public string SMTPUsername
        {
            get { return _sMTPUsername; }
            set { _sMTPUsername = value; }
        }
        private string _sMTPPass = "";
        /// <summary>
        /// Get or Set password related email that enable us from send email. 
        /// </summary>
        public string SMTPPass
        {
            get { return _sMTPPass; }
            set { _sMTPPass = value; }
        }

        private string _senderName = "";
        /// <summary>
        /// Get or Set Name of sender Email.
        /// </summary>
        public string SenderName
        {
            get { return _senderName; }
            set { _senderName = value; }
        }
      
        //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [Start]
        public string SenderEmail { get; set; }
        //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [End]

        private bool _enableSsl = true;
        /// <summary>
        /// Get or Set Ssl(Secure Sockets Layer) <c>Depend on Email provider</c>.
        /// </summary>
        public bool EnableSsl
        {
            get { return _enableSsl; }
            set { _enableSsl = value; }
        }

        /// <summary>
        /// Used to Send email to multi email address and attach multi files and support body of message HTML or not
        /// </summary>
        /// <example>
        /// <code>
        /// string to = "ahmed.maher@ariany.com;hassan.i@ariany.com";
        /// string Cc = "ahmed.r@ariany.com;hr@ariany.com";
        /// string Bcc = "Omar.r@ariany.comm";
        /// string Attach = "C:\\x.txt;D:\\yy.txt";
        /// .....
        /// AriaMessagingManager email = new AriaMessagingManager();
        /// email.SMTPServer = "smtp.gmail.com";
        /// email.SMTPPort = 587;
        /// email.SMTPUsername = "ahmed.maher@ariany.com";
        /// email.SMTPPass = "ahmed";
        /// email.SenderName = "Ahmed Maher Keshk";
        /// email.EnableSsl = true;
        /// email.Aria4SendEmail(to,Cc,Bcc,Attach);
        /// .....
        /// //To Check Error
        /// if(email.Error)                       //check if error
        ///   MessageBoc.Show(email.ErrorMessage);//To Print Message
        /// SenderEmail.ReleaseObj();
        /// .....
        /// //Call From <c>Fox</c>
        /// SenderEmail = CreateObject("Aria.EnterpriseServices.Messaging.AriaMessagingManager")
        /// to = "ahmed.maher@ariany.com;hassan.i@ariany.com"
        /// Cc = "ahmed.r@ariany.com;hr@ariany.com"
        /// Bcc = "Omar.r@ariany.comm"
        /// Attach = "C:\\x.txt;D:\\yy.txt"
        /// 
        /// SenderEmail.SMTPServer = "smtp.gmail.com"
        /// SenderEmail.SMTPPort = 587
        /// SenderEmail.SMTPUsername = "ahmed.maher@ariany.com"
        /// SenderEmail.SenderName = "Ahmed Maher Keshk"
        /// SenderEmail.SMTPPass= "ahmed"
        /// SenderEmail.EnableSsl = .T.
        /// SenderEmail.Aria4SendEmail(to,Cc,Bcc,Attach)
        /// //To Check Error
        /// if(SenderEmail.Error)                       //check if error
        ///   ?SenderEmail.ErrorMessage//To Print Message
        /// SenderEmail.ReleaseObj()
        /// </code>
        /// </example>
        /// <param name="To"></param>
        /// <param name="Cc"></param>
        /// <param name="Bcc"></param>
        /// <param name="Attach"></param>
        public void Aria4SendEmail(string To, string Cc, string Bcc, string Attach)
        {
            try
            {
                smtp.Port = _sMTPPort;
                smtp.Host = _sMTPServer;
                smtp.EnableSsl = _enableSsl;
                smtp.Credentials = new NetworkCredential(_sMTPUsername, _sMTPPass);
                string[] ToArray;
                string[] CcArray;
                string[] BccArray;
                string[] Attachment;
                if (To.Length > 0)
                {
                    ToArray = To.Split(';');
                    for (int i = 0; i < ToArray.Length; i++)
                        mailMessage.To.Add(new MailAddress(ToArray[i]));
                }
                if (Cc.Length > 0)
                {
                    CcArray = Cc.Split(';');
                    for (int i = 0; i < CcArray.Length; i++)
                        mailMessage.CC.Add(new MailAddress(CcArray[i]));
                }
                if (Bcc.Length > 0)
                {
                    BccArray = Bcc.Split(';');
                    for (int i = 0; i < BccArray.Length; i++)
                        mailMessage.Bcc.Add(new MailAddress(BccArray[i]));
                }
                if (Attach.Length > 0)
                {
                    Attachment = Attach.Split(';');
                    for (int i = 0; i < Attachment.Length; i++)
                        mailMessage.Attachments.Add(new Attachment(Attachment[i]));
                }

                //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [Start]
                //mailMessage.From = new MailAddress(_sMTPUsername, _senderName, Encoding.UTF8);
                mailMessage.From = new MailAddress(SenderEmail, _senderName, Encoding.UTF8);
                //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [End]

                mailMessage.IsBodyHtml = _isBodyHtmal;
                mailMessage.Body = _emailBody;
                mailMessage.Subject = _emailSubject;

                smtp.Send(mailMessage);
                _error = false;
                _errorMessage = "";
            }
            catch (Exception Ex)
            {
                _error = true;
                _errorMessage = Ex.Message;
            }
        }

        /// <summary>
        /// Used To release object before use it again.
        /// </summary>
        /// <remarks>
        /// if you send email and attach file and try to send email in same environment where attach that 
        /// same file you will appear error <c>Access deny</c> this beacuse creatd object is hold file,
        /// so must release object before to send again.
        /// </remarks>
        public void ReleaseObj()
        {
            mailMessage.Attachments.Dispose();
        }
        //[END]

        public void SendSerializedEmail(string xmlStringAriaEmail, string clientId)
        {
            AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();

            AriaEmail email = (AriaEmail)xmlSerializer.ConvertFromXml(xmlStringAriaEmail);

            SendEmail(email,clientId);
        }

        public void SendEmailCollection(AriaDbConnection connection, AriaArgumentList argumentList, AriaEmail[] mailMessages, string clientId)
        {
            for (int index = 0; mailMessages != null && index < mailMessages.Length; index++)
            {
                SendEmail(connection, argumentList, mailMessages[index],clientId);
            }
        }
        
        public override object InitializeLifetimeService()
        {
            return null;
        }
    }
}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   