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
	public class TrackReply
	{
		private NotificationSeverityType highestSeverityField;

		private Notification[] notificationsField;

		private  TransactionDetail transactionDetailField;

		private VersionId versionField;

		private bool duplicateWaybillField;

		private bool duplicateWaybillFieldSpecified;

		private bool moreDataField;

		private bool moreDataFieldSpecified;

		private string pagingTokenField;

		private TrackDetail[] trackDetailsField;

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

		public bool MoreData
		{
			get
			{
				return this.moreDataField;
			}
			set
			{
				this.moreDataField = value;
			}
		}

		[XmlIgnore]
		public bool MoreDataSpecified
		{
			get
			{
				return this.moreDataFieldSpecified;
			}
			set
			{
				this.moreDataFieldSpecified = value;
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

		[XmlElement("TrackDetails")]
		public TrackDetail[] TrackDetails
		{
			get
			{
				return this.trackDetailsField;
			}
			set
			{
				this.trackDetailsField = value;
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

		public TrackReply()
		{
		}
	}
}