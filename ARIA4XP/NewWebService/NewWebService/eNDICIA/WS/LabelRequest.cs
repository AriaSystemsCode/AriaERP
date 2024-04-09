using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="www.envmgr.com/LabelService")]
	public class LabelRequest : DataValidator
	{
		private string requesterIDField;

		private string accountIDField;

		private string passPhraseField;

		private string mailClassField;

		private int dateAdvanceField;

		private double weightOzField;

		private string mailpieceShapeField;

		private Dimensions mailpieceDimensionsField;

		private string automationRateField;

		private string machinableField;

		private string serviceLevelField;

		private string sundayHolidayDeliveryField;

		private string sortTypeField;

		private string includePostageField;

		private string replyPostageField;

		private string showReturnAddressField;

		private string stealthField;

		private string validateAddressField;

		private string signatureWaiverField;

		private string noWeekendDeliveryField;

		private SpecialServices servicesField;

		private string trackingNumberField;

		private int costCenterField;

		private float valueField;

		private double cODAmountField;

		private string insuredValueField;

		private double registeredMailValueField;

		private string descriptionField;

		private string customsFormTypeField;

		private string customsFormImageFormatField;

		private string customsFormImageResolutionField;

		private string originCountryField;

		private string contentsTypeField;

		private string contentsExplanationField;

		private string nonDeliveryOptionField;

		private string referenceIDField;

		private string partnerCustomerIDField;

		private string partnerTransactionIDField;

		private string bpodClientDunsNumberField;

		private string rubberStamp1Field;

		private string rubberStamp2Field;

		private string rubberStamp3Field;

		private string entryFacilityField;

		private string pOZipCodeField;

		private string shipDateField;

		private string shipTimeField;

		private string eelPfcField;

		private string customsCertifyField;

		private string customsSignerField;

		private ResponseOptions responseOptionsField;

		private string fromNameField;

		private string fromCompanyField;

		private string returnAddress1Field;

		private string returnAddress2Field;

		private string returnAddress3Field;

		private string returnAddress4Field;

		private string fromCityField;

		private string fromStateField;

		private string fromPostalCodeField;

		private string fromZIP4Field;

		private string fromCountryField;

		private string fromPhoneField;

		private string fromEMailField;

		private string toNameField;

		private string toCompanyField;

		private string toAddress1Field;

		private string toAddress2Field;

		private string toAddress3Field;

		private string toAddress4Field;

		private string toCityField;

		private string toStateField;

		private string toPostalCodeField;

		private string toZIP4Field;

		private string toDeliveryPointField;

		private string toCountryField;

		private string toPhoneField;

		private string toEMailField;

		private string customsCountry1Field;

		private string customsDescription1Field;

		private uint customsQuantity1Field;

		private float customsValue1Field;

		private uint customsWeight1Field;

		private string customsCountry2Field;

		private string customsDescription2Field;

		private uint customsQuantity2Field;

		private float customsValue2Field;

		private uint customsWeight2Field;

		private string customsCountry3Field;

		private string customsDescription3Field;

		private uint customsQuantity3Field;

		private float customsValue3Field;

		private uint customsWeight3Field;

		private string customsCountry4Field;

		private string customsDescription4Field;

		private uint customsQuantity4Field;

		private float customsValue4Field;

		private uint customsWeight4Field;

		private string customsCountry5Field;

		private string customsDescription5Field;

		private uint customsQuantity5Field;

		private float customsValue5Field;

		private uint customsWeight5Field;

		private string testField;

		private string labelTypeField;

		private string labelSubtypeField;

		private string labelSizeField;

		private string imageFormatField;

		private string imageResolutionField;

		private string imageRotationField;

		public string AccountID
		{
			get
			{
				return this.accountIDField;
			}
			set
			{
				this.accountIDField = value;
			}
		}

		public string AutomationRate
		{
			get
			{
				return this.automationRateField;
			}
			set
			{
				this.automationRateField = value;
			}
		}

		public string BpodClientDunsNumber
		{
			get
			{
				return this.bpodClientDunsNumberField;
			}
			set
			{
				this.bpodClientDunsNumberField = value;
			}
		}

		[DefaultValue(0)]
		public double CODAmount
		{
			get
			{
				return this.cODAmountField;
			}
			set
			{
				this.cODAmountField = value;
			}
		}

		public string ContentsExplanation
		{
			get
			{
				return this.contentsExplanationField;
			}
			set
			{
				this.contentsExplanationField = value;
			}
		}

		public string ContentsType
		{
			get
			{
				return this.contentsTypeField;
			}
			set
			{
				this.contentsTypeField = value;
			}
		}

		public int CostCenter
		{
			get
			{
				return this.costCenterField;
			}
			set
			{
				this.costCenterField = value;
			}
		}

		public string CustomsCertify
		{
			get
			{
				return this.customsCertifyField;
			}
			set
			{
				this.customsCertifyField = value;
			}
		}

		public string CustomsCountry1
		{
			get
			{
				return this.customsCountry1Field;
			}
			set
			{
				this.customsCountry1Field = value;
			}
		}

		public string CustomsCountry2
		{
			get
			{
				return this.customsCountry2Field;
			}
			set
			{
				this.customsCountry2Field = value;
			}
		}

		public string CustomsCountry3
		{
			get
			{
				return this.customsCountry3Field;
			}
			set
			{
				this.customsCountry3Field = value;
			}
		}

		public string CustomsCountry4
		{
			get
			{
				return this.customsCountry4Field;
			}
			set
			{
				this.customsCountry4Field = value;
			}
		}

		public string CustomsCountry5
		{
			get
			{
				return this.customsCountry5Field;
			}
			set
			{
				this.customsCountry5Field = value;
			}
		}

		public string CustomsDescription1
		{
			get
			{
				return this.customsDescription1Field;
			}
			set
			{
				this.customsDescription1Field = value;
			}
		}

		public string CustomsDescription2
		{
			get
			{
				return this.customsDescription2Field;
			}
			set
			{
				this.customsDescription2Field = value;
			}
		}

		public string CustomsDescription3
		{
			get
			{
				return this.customsDescription3Field;
			}
			set
			{
				this.customsDescription3Field = value;
			}
		}

		public string CustomsDescription4
		{
			get
			{
				return this.customsDescription4Field;
			}
			set
			{
				this.customsDescription4Field = value;
			}
		}

		public string CustomsDescription5
		{
			get
			{
				return this.customsDescription5Field;
			}
			set
			{
				this.customsDescription5Field = value;
			}
		}

		public string CustomsFormImageFormat
		{
			get
			{
				return this.customsFormImageFormatField;
			}
			set
			{
				this.customsFormImageFormatField = value;
			}
		}

		public string CustomsFormImageResolution
		{
			get
			{
				return this.customsFormImageResolutionField;
			}
			set
			{
				this.customsFormImageResolutionField = value;
			}
		}

		public string CustomsFormType
		{
			get
			{
				return this.customsFormTypeField;
			}
			set
			{
				this.customsFormTypeField = value;
			}
		}

		public uint CustomsQuantity1
		{
			get
			{
				return this.customsQuantity1Field;
			}
			set
			{
				this.customsQuantity1Field = value;
			}
		}

		public uint CustomsQuantity2
		{
			get
			{
				return this.customsQuantity2Field;
			}
			set
			{
				this.customsQuantity2Field = value;
			}
		}

		public uint CustomsQuantity3
		{
			get
			{
				return this.customsQuantity3Field;
			}
			set
			{
				this.customsQuantity3Field = value;
			}
		}

		public uint CustomsQuantity4
		{
			get
			{
				return this.customsQuantity4Field;
			}
			set
			{
				this.customsQuantity4Field = value;
			}
		}

		public uint CustomsQuantity5
		{
			get
			{
				return this.customsQuantity5Field;
			}
			set
			{
				this.customsQuantity5Field = value;
			}
		}

		public string CustomsSigner
		{
			get
			{
				return this.customsSignerField;
			}
			set
			{
				this.customsSignerField = value;
			}
		}

		public float CustomsValue1
		{
			get
			{
				return this.customsValue1Field;
			}
			set
			{
				this.customsValue1Field = value;
			}
		}

		public float CustomsValue2
		{
			get
			{
				return this.customsValue2Field;
			}
			set
			{
				this.customsValue2Field = value;
			}
		}

		public float CustomsValue3
		{
			get
			{
				return this.customsValue3Field;
			}
			set
			{
				this.customsValue3Field = value;
			}
		}

		public float CustomsValue4
		{
			get
			{
				return this.customsValue4Field;
			}
			set
			{
				this.customsValue4Field = value;
			}
		}

		public float CustomsValue5
		{
			get
			{
				return this.customsValue5Field;
			}
			set
			{
				this.customsValue5Field = value;
			}
		}

		public uint CustomsWeight1
		{
			get
			{
				return this.customsWeight1Field;
			}
			set
			{
				this.customsWeight1Field = value;
			}
		}

		public uint CustomsWeight2
		{
			get
			{
				return this.customsWeight2Field;
			}
			set
			{
				this.customsWeight2Field = value;
			}
		}

		public uint CustomsWeight3
		{
			get
			{
				return this.customsWeight3Field;
			}
			set
			{
				this.customsWeight3Field = value;
			}
		}

		public uint CustomsWeight4
		{
			get
			{
				return this.customsWeight4Field;
			}
			set
			{
				this.customsWeight4Field = value;
			}
		}

		public uint CustomsWeight5
		{
			get
			{
				return this.customsWeight5Field;
			}
			set
			{
				this.customsWeight5Field = value;
			}
		}

		public int DateAdvance
		{
			get
			{
				return this.dateAdvanceField;
			}
			set
			{
				this.dateAdvanceField = value;
			}
		}

		public string Description
		{
			get
			{
				return this.descriptionField;
			}
			set
			{
				this.descriptionField = value;
			}
		}

		public string EelPfc
		{
			get
			{
				return this.eelPfcField;
			}
			set
			{
				this.eelPfcField = value;
			}
		}

		public string EntryFacility
		{
			get
			{
				return this.entryFacilityField;
			}
			set
			{
				this.entryFacilityField = value;
			}
		}

		public string FromCity
		{
			get
			{
				return this.fromCityField;
			}
			set
			{
				this.fromCityField = value;
			}
		}

		public string FromCompany
		{
			get
			{
				return this.fromCompanyField;
			}
			set
			{
				this.fromCompanyField = value;
			}
		}

		public string FromCountry
		{
			get
			{
				return this.fromCountryField;
			}
			set
			{
				this.fromCountryField = value;
			}
		}

		public string FromEMail
		{
			get
			{
				return this.fromEMailField;
			}
			set
			{
				this.fromEMailField = value;
			}
		}

		public string FromName
		{
			get
			{
				return this.fromNameField;
			}
			set
			{
				this.fromNameField = value;
			}
		}

		public string FromPhone
		{
			get
			{
				return this.fromPhoneField;
			}
			set
			{
				this.fromPhoneField = value;
			}
		}

		public string FromPostalCode
		{
			get
			{
				return this.fromPostalCodeField;
			}
			set
			{
				this.fromPostalCodeField = value;
			}
		}

		public string FromState
		{
			get
			{
				return this.fromStateField;
			}
			set
			{
				this.fromStateField = value;
			}
		}

		public string FromZIP4
		{
			get
			{
				return this.fromZIP4Field;
			}
			set
			{
				this.fromZIP4Field = value;
			}
		}

		[XmlAttribute]
		public string ImageFormat
		{
			get
			{
				return this.imageFormatField;
			}
			set
			{
				this.imageFormatField = value;
			}
		}

		[XmlAttribute]
		public string ImageResolution
		{
			get
			{
				return this.imageResolutionField;
			}
			set
			{
				this.imageResolutionField = value;
			}
		}

		[XmlAttribute]
		public string ImageRotation
		{
			get
			{
				return this.imageRotationField;
			}
			set
			{
				this.imageRotationField = value;
			}
		}

		public string IncludePostage
		{
			get
			{
				return this.includePostageField;
			}
			set
			{
				this.includePostageField = value;
			}
		}

		public string InsuredValue
		{
			get
			{
				return this.insuredValueField;
			}
			set
			{
				this.insuredValueField = value;
			}
		}

		[XmlAttribute]
		public string LabelSize
		{
			get
			{
				return this.labelSizeField;
			}
			set
			{
				this.labelSizeField = value;
			}
		}

		[XmlAttribute]
		public string LabelSubtype
		{
			get
			{
				return this.labelSubtypeField;
			}
			set
			{
				this.labelSubtypeField = value;
			}
		}

		[XmlAttribute]
		public string LabelType
		{
			get
			{
				return this.labelTypeField;
			}
			set
			{
				this.labelTypeField = value;
			}
		}

		public string Machinable
		{
			get
			{
				return this.machinableField;
			}
			set
			{
				this.machinableField = value;
			}
		}

		public string MailClass
		{
			get
			{
				return this.mailClassField;
			}
			set
			{
				this.mailClassField = value;
			}
		}

		public Dimensions MailpieceDimensions
		{
			get
			{
				return this.mailpieceDimensionsField;
			}
			set
			{
				this.mailpieceDimensionsField = value;
			}
		}

		public string MailpieceShape
		{
			get
			{
				return this.mailpieceShapeField;
			}
			set
			{
				this.mailpieceShapeField = value;
			}
		}

		public string NonDeliveryOption
		{
			get
			{
				return this.nonDeliveryOptionField;
			}
			set
			{
				this.nonDeliveryOptionField = value;
			}
		}

		public string NoWeekendDelivery
		{
			get
			{
				return this.noWeekendDeliveryField;
			}
			set
			{
				this.noWeekendDeliveryField = value;
			}
		}

		public string OriginCountry
		{
			get
			{
				return this.originCountryField;
			}
			set
			{
				this.originCountryField = value;
			}
		}

		public string PartnerCustomerID
		{
			get
			{
				return this.partnerCustomerIDField;
			}
			set
			{
				this.partnerCustomerIDField = value;
			}
		}

		public string PartnerTransactionID
		{
			get
			{
				return this.partnerTransactionIDField;
			}
			set
			{
				this.partnerTransactionIDField = value;
			}
		}

		public string PassPhrase
		{
			get
			{
				return this.passPhraseField;
			}
			set
			{
				this.passPhraseField = value;
			}
		}

		public string POZipCode
		{
			get
			{
				return this.pOZipCodeField;
			}
			set
			{
				this.pOZipCodeField = value;
			}
		}

		public string ReferenceID
		{
			get
			{
				return this.referenceIDField;
			}
			set
			{
				this.referenceIDField = value;
			}
		}

		[DefaultValue(0)]
		public double RegisteredMailValue
		{
			get
			{
				return this.registeredMailValueField;
			}
			set
			{
				this.registeredMailValueField = value;
			}
		}

		public string ReplyPostage
		{
			get
			{
				return this.replyPostageField;
			}
			set
			{
				this.replyPostageField = value;
			}
		}

		public string RequesterID
		{
			get
			{
				return this.requesterIDField;
			}
			set
			{
				this.requesterIDField = value;
			}
		}

		public ResponseOptions ResponseOptions
		{
			get
			{
				return this.responseOptionsField;
			}
			set
			{
				this.responseOptionsField = value;
			}
		}

		public string ReturnAddress1
		{
			get
			{
				return this.returnAddress1Field;
			}
			set
			{
				this.returnAddress1Field = value;
			}
		}

		public string ReturnAddress2
		{
			get
			{
				return this.returnAddress2Field;
			}
			set
			{
				this.returnAddress2Field = value;
			}
		}

		public string ReturnAddress3
		{
			get
			{
				return this.returnAddress3Field;
			}
			set
			{
				this.returnAddress3Field = value;
			}
		}

		public string ReturnAddress4
		{
			get
			{
				return this.returnAddress4Field;
			}
			set
			{
				this.returnAddress4Field = value;
			}
		}

		public string RubberStamp1
		{
			get
			{
				return this.rubberStamp1Field;
			}
			set
			{
				this.rubberStamp1Field = value;
			}
		}

		public string RubberStamp2
		{
			get
			{
				return this.rubberStamp2Field;
			}
			set
			{
				this.rubberStamp2Field = value;
			}
		}

		public string RubberStamp3
		{
			get
			{
				return this.rubberStamp3Field;
			}
			set
			{
				this.rubberStamp3Field = value;
			}
		}

		public string ServiceLevel
		{
			get
			{
				return this.serviceLevelField;
			}
			set
			{
				this.serviceLevelField = value;
			}
		}

		public SpecialServices Services
		{
			get
			{
				return this.servicesField;
			}
			set
			{
				this.servicesField = value;
			}
		}

		public string ShipDate
		{
			get
			{
				return this.shipDateField;
			}
			set
			{
				this.shipDateField = value;
			}
		}

		public string ShipTime
		{
			get
			{
				return this.shipTimeField;
			}
			set
			{
				this.shipTimeField = value;
			}
		}

		public string ShowReturnAddress
		{
			get
			{
				return this.showReturnAddressField;
			}
			set
			{
				this.showReturnAddressField = value;
			}
		}

		public string SignatureWaiver
		{
			get
			{
				return this.signatureWaiverField;
			}
			set
			{
				this.signatureWaiverField = value;
			}
		}

		public string SortType
		{
			get
			{
				return this.sortTypeField;
			}
			set
			{
				this.sortTypeField = value;
			}
		}

		public string Stealth
		{
			get
			{
				return this.stealthField;
			}
			set
			{
				this.stealthField = value;
			}
		}

		public string SundayHolidayDelivery
		{
			get
			{
				return this.sundayHolidayDeliveryField;
			}
			set
			{
				this.sundayHolidayDeliveryField = value;
			}
		}

		[XmlAttribute]
		public string Test
		{
			get
			{
				return this.testField;
			}
			set
			{
				this.testField = value;
			}
		}

		public string ToAddress1
		{
			get
			{
				return this.toAddress1Field;
			}
			set
			{
				this.toAddress1Field = value;
			}
		}

		public string ToAddress2
		{
			get
			{
				return this.toAddress2Field;
			}
			set
			{
				this.toAddress2Field = value;
			}
		}

		public string ToAddress3
		{
			get
			{
				return this.toAddress3Field;
			}
			set
			{
				this.toAddress3Field = value;
			}
		}

		public string ToAddress4
		{
			get
			{
				return this.toAddress4Field;
			}
			set
			{
				this.toAddress4Field = value;
			}
		}

		public string ToCity
		{
			get
			{
				return this.toCityField;
			}
			set
			{
				this.toCityField = value;
			}
		}

		public string ToCompany
		{
			get
			{
				return this.toCompanyField;
			}
			set
			{
				this.toCompanyField = value;
			}
		}

		public string ToCountry
		{
			get
			{
				return this.toCountryField;
			}
			set
			{
				this.toCountryField = value;
			}
		}

		public string ToDeliveryPoint
		{
			get
			{
				return this.toDeliveryPointField;
			}
			set
			{
				this.toDeliveryPointField = value;
			}
		}

		public string ToEMail
		{
			get
			{
				return this.toEMailField;
			}
			set
			{
				this.toEMailField = value;
			}
		}

		public string ToName
		{
			get
			{
				return this.toNameField;
			}
			set
			{
				this.toNameField = value;
			}
		}

		public string ToPhone
		{
			get
			{
				return this.toPhoneField;
			}
			set
			{
				this.toPhoneField = value;
			}
		}

		public string ToPostalCode
		{
			get
			{
				return this.toPostalCodeField;
			}
			set
			{
				this.toPostalCodeField = value;
			}
		}

		public string ToState
		{
			get
			{
				return this.toStateField;
			}
			set
			{
				this.toStateField = value;
			}
		}

		public string ToZIP4
		{
			get
			{
				return this.toZIP4Field;
			}
			set
			{
				this.toZIP4Field = value;
			}
		}

		public string TrackingNumber
		{
			get
			{
				return this.trackingNumberField;
			}
			set
			{
				this.trackingNumberField = value;
			}
		}

		public string ValidateAddress
		{
			get
			{
				return this.validateAddressField;
			}
			set
			{
				this.validateAddressField = value;
			}
		}

		public float Value
		{
			get
			{
				return this.valueField;
			}
			set
			{
				this.valueField = value;
			}
		}

		public double WeightOz
		{
			get
			{
				return this.weightOzField;
			}
			set
			{
				this.weightOzField = value;
			}
		}

		public LabelRequest()
		{
			this.cODAmountField = 0;
			this.registeredMailValueField = 0;
		}
	}
}