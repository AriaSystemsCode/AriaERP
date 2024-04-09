using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.Messaging;

namespace Aria.ParameterSubstitution
{
    public class AriaParameterizedEmail : AriaParameterizedItem
    {

        public AriaParameterizedEmail(AriaArgumentList arguments)
            : base(arguments)
        {
        }

        public void Substitute(AriaEmail email)
        {
            AriaParameterizedText parameterizedText = new AriaParameterizedText(Arguments);
            email.To = parameterizedText.Substitute(email.To);
            email.Cc = parameterizedText.Substitute(email.Cc);
            email.Bcc = parameterizedText.Substitute(email.Bcc);

            //for (int attachmentIndex = 0; attachmentIndex < email.Attachment.Count; attachmentIndex++)
            //{
            //    email.Attachment[attachmentIndex] = parameterizedText.Substitute((string)email.Attachment[attachmentIndex]);
            //}

            email.Subject = parameterizedText.Substitute(email.Subject);
            email.Body = parameterizedText.Substitute(email.Body);
        }
    }
}
