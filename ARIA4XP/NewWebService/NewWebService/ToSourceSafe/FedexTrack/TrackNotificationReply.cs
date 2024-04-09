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
	public class TrackNotificationReply
	{
		private NotificationSeverityType highestSeverityField;

		private Notification[] notificationsField;

		private  TransactionDetail transactionDetailField;

		private VersionId versionField;

		private bool duplicateWaybillField;

		private bool duplicateWaybillFieldSpecified;

		private bool moreDataAvailableField;

		private bool moreDataAvailableFieldSpecified;

		private string pagingTokenField;

		private TrackNotificationPackage[] packagesField;

		public bool DuplicateWaybill
		{
			get
			{
				return this.duplicateWaybillField;
			}
			set
			{
				this.duplicateWaybillField = value;
			}
		}

		[XmlIgnore]
		public bool DuplicateWaybillSpecified
		{
			get
			{
				return this.duplicateWaybillFieldSpecified;
			}
			set
			{
				this.duplicateWaybillFieldSpecified = value;
			}
		}

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

		public bool MoreDataAvailable
		{
			get
			{
				return this.moreDataAvailableField;
			}
			set
			{
				this.moreDataAvailableField = value;
			}
		}

		[XmlIgnore]
		public bool MoreDataAvailableSpecified
		{
			get
			{
				return this.moreDataAvailableFieldSpecified;
			}
			set
			{
				this.moreDataAvailableFieldSpecified = value;
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

		[XmlElement("Packages")]
		public TrackNotificationPackage[] Packages
		{
			get
			{
				return this.packagesField;
			}
			set
			{
				this.packagesField = value;
			}
		}

		public string PagingToken
		{
			get
			{
				return this.pagingTokenField;
			}
			set
			{
				this.pagingTokenField = value;
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

		public TrackNotificationReply()
		{
		}
	}
}