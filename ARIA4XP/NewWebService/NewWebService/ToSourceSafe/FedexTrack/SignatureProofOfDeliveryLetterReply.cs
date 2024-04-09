using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public class SignatureProofOfDeliveryLetterReply
	{
		private NotificationSeverityType highestSeverityField;

		private Notification[] notificationsField;

		private  TransactionDetail transactionDetailField;

		private VersionId versionField;

		private byte[] letterField;

		public NotificationSeverityType HighestSeverity
		{
			get
			{
				return this.highestSeverityField;
			}
			set
			{
				this.highestSeverityField = value;
			}
		}

		[XmlElement(DataType="base64Binary")]
		public byte[] Letter
		{
			get
			{
				return this.letterField;
			}
			set
			{
				this.letterField = value;
			}
		}

		[XmlElement("Notifications")]
		public Notification[] Notifications
		{
			get
			{
				return this.notificationsField;
			}
			set
			{
				this.notificationsField = value;
			}
		}

		public  TransactionDetail TransactionDetail
		{
			get
			{
				return this.transactionDetailField;
			}
			set
			{
				this.transactionDetailField = value;
			}
		}

		public VersionId Version
		{
			get
			{
				return this.versionField;
			}
			set
			{
				this.versionField = value;
			}
		}

		public SignatureProofOfDeliveryLetterReply()
		{
		}
	}
}