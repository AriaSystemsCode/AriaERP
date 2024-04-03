
/****** Object:  Default [DF_Attachment_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[Attachment] ADD  CONSTRAINT [DF_Attachment_oid]  DEFAULT (newid()) FOR [oid]
/****** Object:  Default [DF_AttachmentCategory_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[AttachmentCategory] ADD  CONSTRAINT [DF_AttachmentCategory_oid]  DEFAULT (newid()) FOR [oid]
/****** Object:  Default [DF_Entity_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[Entity] ADD  CONSTRAINT [DF_Entity_oid]  DEFAULT (newid()) FOR [oid]
/****** Object:  Default [DF_Entity_entitytypeoid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[Entity] ADD  CONSTRAINT [DF_Entity_entitytypeoid]  DEFAULT (newid()) FOR [type]

/****** Object:  Default [DF_EntityAttachment_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityAttachment] ADD  CONSTRAINT [DF_EntityAttachment_oid]  DEFAULT (newid()) FOR [oid]

/****** Object:  Default [DF_EntityCategory_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityCategory] ADD  CONSTRAINT [DF_EntityCategory_oid]  DEFAULT (newid()) FOR [oid]

/****** Object:  Default [DF_EntityClassification_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityClassification] ADD  CONSTRAINT [DF_EntityClassification_oid]  DEFAULT (newid()) FOR [oid]

/****** Object:  Default [DF_EntityStatus_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityStatus] ADD  CONSTRAINT [DF_EntityStatus_oid]  DEFAULT (newid()) FOR [oid]

/****** Object:  Default [DF_EntityType_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityType] ADD  CONSTRAINT [DF_EntityType_oid]  DEFAULT (newid()) FOR [oid]

/****** Object:  Default [DF_EntityTypeRelationship_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityTypeRelationship] ADD  CONSTRAINT [DF_EntityTypeRelationship_oid]  DEFAULT (newid()) FOR [oid]

/****** Object:  Default [DF_EntityTypeSettings_oid]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityTypeSettings] ADD  CONSTRAINT [DF_EntityTypeSettings_oid]  DEFAULT (newid()) FOR [oid]

/****** Object:  Default [DF__IdentifierS__OID__7AE7E7AD]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierSegment] ADD  DEFAULT (newid()) FOR [OID]

/****** Object:  Default [DF__IdentifierS__OID__00A0C103]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierSegmentFormat] ADD  DEFAULT (newid()) FOR [OID]

/****** Object:  Default [DF__IdentifierS__OID__06599A59]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierSegmentValidEntry] ADD  DEFAULT (newid()) FOR [OID]

/****** Object:  Default [DF__IdentifierS__OID__7346C5E5]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierStructure] ADD  DEFAULT (newid()) FOR [OID]

/****** Object:  ForeignKey [FK_AttachmentCategory_ParentCategory]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[AttachmentCategory]  WITH CHECK ADD  CONSTRAINT [FK_AttachmentCategory_ParentCategory] FOREIGN KEY([ParentCategory])
REFERENCES [dbo].[AttachmentCategory] ([oid])

ALTER TABLE [dbo].[AttachmentCategory] CHECK CONSTRAINT [FK_AttachmentCategory_ParentCategory]

/****** Object:  ForeignKey [FK_Entity_EntityCategory]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[Entity]  WITH CHECK ADD  CONSTRAINT [FK_Entity_EntityCategory] FOREIGN KEY([Category])
REFERENCES [dbo].[EntityCategory] ([oid])

ALTER TABLE [dbo].[Entity] CHECK CONSTRAINT [FK_Entity_EntityCategory]

/****** Object:  ForeignKey [FK_Entity_EntityClassification]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[Entity]  WITH CHECK ADD  CONSTRAINT [FK_Entity_EntityClassification] FOREIGN KEY([Classification])
REFERENCES [dbo].[EntityClassification] ([oid])

ALTER TABLE [dbo].[Entity] CHECK CONSTRAINT [FK_Entity_EntityClassification]

/****** Object:  ForeignKey [FK_Entity_EntityStatus]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[Entity]  WITH CHECK ADD  CONSTRAINT [FK_Entity_EntityStatus] FOREIGN KEY([status])
REFERENCES [dbo].[EntityStatus] ([oid])

ALTER TABLE [dbo].[Entity] CHECK CONSTRAINT [FK_Entity_EntityStatus]

/****** Object:  ForeignKey [FK_Entity_EntityType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[Entity]  WITH CHECK ADD  CONSTRAINT [FK_Entity_EntityType] FOREIGN KEY([type])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[Entity] CHECK CONSTRAINT [FK_Entity_EntityType]

/****** Object:  ForeignKey [FK_EntityAttachment_Attachment]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityAttachment]  WITH CHECK ADD  CONSTRAINT [FK_EntityAttachment_Attachment] FOREIGN KEY([Attachment])
REFERENCES [dbo].[Attachment] ([oid])

ALTER TABLE [dbo].[EntityAttachment] CHECK CONSTRAINT [FK_EntityAttachment_Attachment]

/****** Object:  ForeignKey [FK_EntityAttachment_AttachmentCategory]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityAttachment]  WITH CHECK ADD  CONSTRAINT [FK_EntityAttachment_AttachmentCategory] FOREIGN KEY([AttachmentCategory])
REFERENCES [dbo].[AttachmentCategory] ([oid])

ALTER TABLE [dbo].[EntityAttachment] CHECK CONSTRAINT [FK_EntityAttachment_AttachmentCategory]

/****** Object:  ForeignKey [FK_EntityAttachment_Entity]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityAttachment]  WITH CHECK ADD  CONSTRAINT [FK_EntityAttachment_Entity] FOREIGN KEY([entity])
REFERENCES [dbo].[Entity] ([oid])

ALTER TABLE [dbo].[EntityAttachment] CHECK CONSTRAINT [FK_EntityAttachment_Entity]

/****** Object:  ForeignKey [FK_EntityCategory_EntityType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityCategory]  WITH CHECK ADD  CONSTRAINT [FK_EntityCategory_EntityType] FOREIGN KEY([EntityType])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[EntityCategory] CHECK CONSTRAINT [FK_EntityCategory_EntityType]

/****** Object:  ForeignKey [FK_EntityCategory_ParentCategory]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityCategory]  WITH CHECK ADD  CONSTRAINT [FK_EntityCategory_ParentCategory] FOREIGN KEY([ParentCategory])
REFERENCES [dbo].[EntityCategory] ([oid])

ALTER TABLE [dbo].[EntityCategory] CHECK CONSTRAINT [FK_EntityCategory_ParentCategory]

/****** Object:  ForeignKey [FK_EntityCategory_ParentClassification]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityClassification]  WITH CHECK ADD  CONSTRAINT [FK_EntityCategory_ParentClassification] FOREIGN KEY([ParentClassification])
REFERENCES [dbo].[EntityClassification] ([oid])

ALTER TABLE [dbo].[EntityClassification] CHECK CONSTRAINT [FK_EntityCategory_ParentClassification]

/****** Object:  ForeignKey [FK_EntityClassification_EntityType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityClassification]  WITH CHECK ADD  CONSTRAINT [FK_EntityClassification_EntityType] FOREIGN KEY([EntityType])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[EntityClassification] CHECK CONSTRAINT [FK_EntityClassification_EntityType]

/****** Object:  ForeignKey [FK_EntityStatus_EntityStatus]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityStatus]  WITH CHECK ADD  CONSTRAINT [FK_EntityStatus_EntityStatus] FOREIGN KEY([oid])
REFERENCES [dbo].[EntityStatus] ([oid])

ALTER TABLE [dbo].[EntityStatus] CHECK CONSTRAINT [FK_EntityStatus_EntityStatus]

/****** Object:  ForeignKey [FK_EntityStatus_EntityType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityStatus]  WITH CHECK ADD  CONSTRAINT [FK_EntityStatus_EntityType] FOREIGN KEY([EntityType])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[EntityStatus] CHECK CONSTRAINT [FK_EntityStatus_EntityType]

/****** Object:  ForeignKey [FK_EntityType_IdentifierStructure]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityType]  WITH CHECK ADD  CONSTRAINT [FK_EntityType_IdentifierStructure] FOREIGN KEY([IdentifierStructure])
REFERENCES [dbo].[IdentifierStructure] ([OID])

ALTER TABLE [dbo].[EntityType] CHECK CONSTRAINT [FK_EntityType_IdentifierStructure]

/****** Object:  ForeignKey [FK_EntityType_ParentType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityType]  WITH CHECK ADD  CONSTRAINT [FK_EntityType_ParentType] FOREIGN KEY([parenttype])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[EntityType] CHECK CONSTRAINT [FK_EntityType_ParentType]

/****** Object:  ForeignKey [FK_EntityTypeRelationship_EntityType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityTypeRelationship]  WITH CHECK ADD  CONSTRAINT [FK_EntityTypeRelationship_EntityType] FOREIGN KEY([EntityType])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[EntityTypeRelationship] CHECK CONSTRAINT [FK_EntityTypeRelationship_EntityType]

/****** Object:  ForeignKey [FK_EntityTypeRelationship_RelatedType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityTypeRelationship]  WITH CHECK ADD  CONSTRAINT [FK_EntityTypeRelationship_RelatedType] FOREIGN KEY([RelatedType])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[EntityTypeRelationship] CHECK CONSTRAINT [FK_EntityTypeRelationship_RelatedType]

/****** Object:  ForeignKey [FK_EntityTypeSettings_EntityType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[EntityTypeSettings]  WITH CHECK ADD  CONSTRAINT [FK_EntityTypeSettings_EntityType] FOREIGN KEY([entitytypeoid])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[EntityTypeSettings] CHECK CONSTRAINT [FK_EntityTypeSettings_EntityType]

/****** Object:  ForeignKey [FK__Identifie__Ident__7BDC0BE6]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierSegment]  WITH CHECK ADD FOREIGN KEY([IdentifierStructure])
REFERENCES [dbo].[IdentifierStructure] ([OID])

/****** Object:  ForeignKey [FK_IdentifierSegment_EntityType]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierSegment]  WITH CHECK ADD  CONSTRAINT [FK_IdentifierSegment_EntityType] FOREIGN KEY([EntityType])
REFERENCES [dbo].[EntityType] ([oid])

ALTER TABLE [dbo].[IdentifierSegment] CHECK CONSTRAINT [FK_IdentifierSegment_EntityType]

/****** Object:  ForeignKey [FK__Identifie__Ident__0194E53C]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierSegmentFormat]  WITH CHECK ADD FOREIGN KEY([IdentifierSegment])
REFERENCES [dbo].[IdentifierSegment] ([OID])

/****** Object:  ForeignKey [FK__Identifie__Ident__074DBE92]    Script Date: 10/08/2020 13:50:36 ******/
ALTER TABLE [dbo].[IdentifierSegmentValidEntry]  WITH CHECK ADD FOREIGN KEY([IdentifierSegment])
REFERENCES [dbo].[IdentifierSegment] ([OID])


INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'affc9204-d2b5-4eaa-afa6-1a416e9d2c02', N'Vendor                        ', N'Vendor                                                                                              ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'98fe78bc-2535-49d9-ac1b-326baa16b4ba', N'Style                         ', N'Style                                                                                               ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'6bb9c070-0bab-4fe3-a828-35b3e747ab54', N'Invoice                       ', N'AR Invoice                                                                                          ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'd441512d-06d7-4d5c-ac2e-37ca6e590222', N'Imported Style Cost Sheet     ', N'Imported Style Cost Sheet                                                                           ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'0751c5df-f457-4cbf-99d8-4af8d1b5abad', N'Manufactured Style Cost Sheet ', N'Manufactured Style Cost Sheet                                                                       ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'b68d46f0-43ae-4c22-8e9c-53307433b7ef', N'Picking Ticket                ', N'Picking Ticket                                                                                      ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'21d41fb5-2120-4652-bec7-59f0c9c01646', N'Journal Batch                 ', N'Journal Batch                                                                                       ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'2a46da97-0b31-4f9b-9219-68f24a9a9f9e', N'Sales Representative          ', N'Sales Representative                                                                                ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'2c37932b-24f8-4ba2-84dc-6fe41acd5751', N'Sales Order                   ', N'Sales Order                                                                                         ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30', N'Style Purchase Order          ', N'Style Purchase Order                                                                                ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'41ed399b-5775-4b26-a05e-86816df936f7', N'Return Authorization          ', N'Return Authorization                                                                                ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'27d46954-7268-4ec5-b073-9477364af991', N'Cutting ticket                ', N'Cutting ticket                                                                                      ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'a4910c84-08aa-44c7-9c49-9535bceab155', N'Style PO Cost sheet           ', N'Style PO Cost sheet                                                                                 ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8', N'GL Single Transaction         ', N'GL Single Transaction                                                                               ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'ea212aa6-6c7a-49c0-ab76-bec37f2c9547', N'Style PO Shipment             ', N'Style PO Shipment                                                                                   ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'efdefd11-8886-4db4-9b21-c636e915e5a9', N'Credit Memo                   ', N'Credit Memo                                                                                         ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'8a25c4f9-e847-469b-9c21-ca66018350c6', N'Material Purchase Order       ', N'Material Purchase Order                                                                             ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'f8ac2a0c-ab5c-44cc-a1eb-cdcbefe9f8e8', N'Payable Invoice               ', N'Payable Invoice                                                                                     ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'22dff895-c2c0-43b0-9a10-ce2f38fb6307', N'Bill of Lading                ', N'Bill of Lading                                                                                      ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'83fadaa5-4b9a-4140-9798-d89d78cf48f7', N'Customer                      ', N'Customer                                                                                            ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'41aa07fe-f779-4850-a9ab-d991c2c80a32', N'EDI Temporary Sales Order     ', N'EDI Temporary Sales Order                                                                           ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'dae0bde8-3fe2-4898-9104-de1313947ad8', N'Packing List                  ', N'Packing List                                                                                        ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'95e5408c-c8b3-486c-9c7a-e2dd337a5f3e', N'Cutting Ticket Cost Sheet     ', N'Cutting Ticket Cost Sheet                                                                           ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'8b661d81-0644-44af-81e6-ed83ab4071eb', N'Material PO Shipment          ', N'Material PO Shipment                                                                                ', NULL, NULL)
INSERT [dbo].[EntityType] ([oid], [id], [name], [parenttype], [IdentifierStructure]) VALUES (N'1e5b9214-8771-44fb-a6c3-fbf0f101d4f8', N'Material                      ', N'Material                                                                                            ', NULL, NULL)

INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink]) VALUES (N'99d774bc-83e6-4101-94d9-b289f8814376', N'98fe78bc-2535-49d9-ac1b-326baa16b4ba', N'Style                         ', 1, N'CSTYMAJOR,XCOLOR                                                                                    ', N'CSTYMAJOR,XCOLOR                                                                                    ', N'IC', N'ICSTYLE             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Style">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>cStyMajor</Name>
<Description>Style Major</Description>
<DataType>VarChar</DataType>
<Width>19</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Style</Name>
<Description>Style</Description>
<DataType>VarChar</DataType>
<Width>19</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SEASON</Name>
<Description>Season Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cpurcode</Name>
<Description>Purchase Group Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cdisccode</Name>
<Description>Discount Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Scale</Name>
<Description>Scale Code</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cstygroup</Name>
<Description>Style Group Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>pattern</Name>
<Description>Style Pattern</Description>
<DataType>VarChar</DataType>
<Width>10</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cstygrade</Name>
<Description>Style Quality</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>soldout</Name>
<Description>Style Sold Out</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DESC</Name>
<Description>Description</Description>
<DataType>VarChar</DataType>
<Width>30</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DESC1</Name>
<Description>Long Description</Description>
<DataType>VarChar</DataType>
<Width>60</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'STYLE                         ', N'STYLE                         ', N'STYLE                         ', N'STYLE', N'STYLE', N'STYLE                                                                                               ', N'.F.                                                                                                 ', N'Desc                          ', N'''S'',Style                     ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink]) VALUES (N'f48cd1ad-8068-4565-b644-1cf6b818e3c9', N'1e5b9214-8771-44fb-a6c3-fbf0f101d4f8', N'Material                      ', 1, N'CINVTYPE,CSTYMAJOR,XCOLOR                                                                           ', N'''0002'',CSTYMAJOR,XCOLOR                                                                             ', N'MA', N'MAMATRL             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Material">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>cStyMajor</Name>
<Description>Item Major</Description>
<DataType>VarChar</DataType>
<Width>19</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Style</Name>
<Description>Item</Description>
<DataType>VarChar</DataType>
<Width>19</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SEASON</Name>
<Description>Season Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
 <ShowInBrowse>False</ShowInBrowse>
</Attribute>
 <Attribute>
<Name>Status</Name>
<Description>Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cpurcode</Name>
<Description>Purchase Group Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cdisccode</Name>
<Description>Discount Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>ITEM_TYPE</Name>
<Description>Item Type Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description>
<DataType>VarChar</DataType>
<Width>4</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cstygroup</Name>
<Description>Item Group Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>pattern</Name>
<Description>Item Pattern</Description>
<DataType>VarChar</DataType>
<Width>10</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>cstygrade</Name>
<Description>Item Quality</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>soldout</Name>
<Description>Item Sold Out</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DESC</Name>
<Description>Description</Description>
<DataType>VarChar</DataType>
<Width>30</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DESC1</Name>
<Description>Long Description</Description>
<DataType>VarChar</DataType>
<Width>60</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'STYLE                         ', N'ITEM                          ', N'STYLE                         ', N'CINVTYPE,STYLE', N'''0002'',STYLE', N'STYLE                                                                                               ', N'.F.                                                                                                 ', N'Desc                          ', N'''M'',Style                     ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'88e3a225-397d-4b75-a841-30100f085ccc', N'2c37932b-24f8-4ba2-84dc-6fe41acd5751', N'Sales Order                   ', 0, N'CORDTYPE,ORDER                                                                                      ', N'''O'',ORDER                                                                                           ', N'SO', N'SOORD               ', 1, N'', N'SOORCN              ', N'<ExtraAttributes ObjectType="Sales Order">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>Order</Name>
<Description>Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Start</Name>
<Description>Order Start Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Order Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Complete</Name>
<Description>Order Complete Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Order Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CustPO</Name>
<Description>Customer PO Number</Description>
<DataType>VarChar</DataType>
<Width>15</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Rep1</Name>
<Description>Sales RepCode 1</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Rep2</Name>
<Description>Sales Rep Code2</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CCURRCODE</Name>
<Description>Currency Code</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SEASON</Name>
<Description>Season Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SPCINST</Name>
<Description>Special Instructions Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CORDTYPE</Name>
<Description>Order Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>BULK</Name>
<Description>Bulk Order</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CCONTREF</Name>
<Description>Contract Reference</Description>
<DataType>VarChar</DataType>
<Width>30</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'Order', N'ORDHDR                        ', N'ORDHDR                        ', N'CORDTYPE,ORDER', N'''O'',Order', N'ORDER                                                                                               ', N'''O'',.F.,''?''                                                                                         ', N'                              ', N'''O'',ORDER                     ', N'''B'',ORDER                     ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'04aa7fe9-6eff-4375-9f0c-87a1ae77b986', N'41aa07fe-f779-4850-a9ab-d991c2c80a32', N'EDI Temporary Sales Order     ', 0, N'CORDTYPE,ORDER                                                                                      ', N'''T'',ORDER                                                                                           ', N'SO', N'SOORD               ', 0, N'', N'SOORCN              ', N'<ExtraAttributes ObjectType="EDI Temp. Order">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>Order</Name>
<Description>Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Start</Name>
<Description>Order Start Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Order Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Complete</Name>
<Description>Order Complete Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Order Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CustPO</Name>
<Description>Customer PO Number</Description>
<DataType>VarChar</DataType>
<Width>15</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Rep1</Name>
<Description>Sales RepCode 1</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Rep2</Name>
<Description>Sales Rep Code2</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CCURRCODE</Name>
<Description>Currency Code</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SEASON</Name>
<Description>Season Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SPCINST</Name>
<Description>Special Instructions Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CORDTYPE</Name>
<Description>Order Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>BULK</Name>
<Description>Bulk Order</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CCONTREF</Name>
<Description>Contract Reference</Description>
<DataType>VarChar</DataType>
<Width>30</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'Order', N'ORDHDR                        ', N'ORDHDR                        ', N'CORDTYPE,ORDER', N'''T'',Order', N'ORDER                                                                                               ', N'                                                                                                    ', N'                              ', N'''T'',ORDER                     ', N'''B'',ORDER                     ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'04c8672a-9b6e-47f2-b749-cf6d9cdc3f8d', N'83fadaa5-4b9a-4140-9798-d89d78cf48f7', N'Customer                      ', 0, N'ACCOUNT                                                                                             ', N'ACCOUNT                                                                                             ', N'AR', N'ARCUST              ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Customer">  
<ObjectTypeExtraAttributes> 
<Attribute>
<Name>Account</Name> 
<Description>Account</Description> 
<DataType>VarChar</DataType> 
<Width>5</Width>  
<Decimals>0</Decimals>
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>True</ShowInBrowse> 
</Attribute> 
<Attribute>
<Name>Status</Name> 
<Description>Customer Status</Description>  
<DataType>VarChar</DataType> 
<Width>1</Width>
<Decimals>0</Decimals> 
<ValidEntries/>  
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse>  
</Attribute>
<Attribute>
<Name>TYPE</Name> 
<Description>Type</Description>  
<DataType>VarChar</DataType> 
<Width>1</Width>
<Decimals>0</Decimals> 
<ValidEntries/>  
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse>  
</Attribute>
<Attribute>
<Name>PHONE1</Name> 
<Description>Phone 1</Description>  
<DataType>VarChar</DataType> 
<Width>16</Width>
<Decimals>0</Decimals> 
<ValidEntries/>  
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse>  
</Attribute>
<Attribute> 
<Name>CSTOREGLN</Name>
<Description>Store GLN</Description> 
<DataType>VarChar</DataType> 
<Width>13</Width>
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>
<Attribute> 
<Name>STNAME</Name>
<Description>Ship to Name</Description> 
<DataType>VarChar</DataType> 
<Width>30</Width>
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse> 
</Attribute>
<Attribute> 
<Name>Store</Name>
<Description>Customer Store Number</Description> 
<DataType>VarChar</DataType> 
<Width>8</Width>
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute>
<Name>salesrep</Name>
<Description>Sales Rep. Code 1</Description>
<DataType>VarChar</DataType>  
<Width>3</Width>  
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>  
<Attribute>  
<Name>Rep2</Name>
<Description>Sales Rep. Code 2</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>CCURRCODE</Name>  
<Description>Currency Code</Description> 
<DataType>VarChar</DataType>
<Width>3</Width> 
<Decimals>0</Decimals>
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>  
<Attribute> 
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description> 
<DataType>VarChar</DataType>
<Width>6</Width> 
<Decimals>0</Decimals> 
<ValidEntries/>  
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>  
<Attribute> 
<Name>SHIPVIA</Name> 
<Description>Ship Via Code</Description> 
<DataType>VarChar</DataType>
<Width>6</Width> 
<Decimals>0</Decimals> 
<ValidEntries/>  
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>SPCINST</Name> 
<Description>Special Instructions Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>CDIVISION</Name> 
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width> 
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute> 
<Name>REGION</Name> 
<Description>Region Code</Description> 
<DataType>VarChar</DataType>
<Width>6</Width> 
<Decimals>0</Decimals>
<ValidEntries/> 
<DefaultValue/>  
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>  
<Attribute> 
<Name>Priority</Name> 
<Description>Customer Priority</Description> 
<DataType>VarChar</DataType>
<Width>3</Width>  
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>cfaccode</Name> 
<Description>Factor Code</Description>  
<DataType>VarChar</DataType> 
<Width>6</Width> 
<Decimals>0</Decimals>
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute></ObjectTypeExtraAttributes>
</ExtraAttributes>', N'Account', N'Customer                      ', N'Customer                      ', N'TYPE,ACCOUNT,STORE', N'''M'',Account', N'Account,Store                                                                                       ', N'''M'',.F.                                                                                             ', N'BTNAME                        ', N'''A'',ACCOUNT                   ', N'''A'',ACCOUNT                   ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes] ) VALUES (N'fdcbd43b-f4fa-4783-b017-133222c2bb80', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30', N'Style Purchase Order          ', 0, N'CBUSDOCU,CSTYTYPE,PO                                                                                ', N'''P'',''P'',PO                                                                                          ', N'PO', N'POSTY               ', 1, N'', N'POSTYP              ', N'<ExtraAttributes ObjectType="Style Purchase Order">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>PO</Name>
<Description>Purchase Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Vendor</Name>
<Description>Vendor</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Available</Name>
<Description>Purchase Order Available Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Purchase Order Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Complete</Name>
<Description>Purchase Order complete Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Purchase Order Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>		  		  
<Width>6</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>		  		  
<Width>6</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>		  		  
<Width>6</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CPURCODE</Name>
<Description>Purchase Group Code</Description>
<DataType>VarChar</DataType>		  		  
<Width>6</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBUSDOCU</Name>
<Description>Business Document Type</Description>
<DataType>VarChar</DataType>		  		  
<Width>1</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSTYTYPE</Name>
<Description>Style PO Type</Description>
<DataType>VarChar</DataType>		  		  
<Width>1</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CLCNO</Name>
<Description>Letter of Credit Number</Description>
<DataType>VarChar</DataType>		  		  
<Width>6</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description>
<DataType>VarChar</DataType>		  		  
<Width>4</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CMRP</Name>
<Description>MRP Number</Description>
<DataType>VarChar</DataType>		  		  
<Width>6</Width>		  		  		  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'PO', N'POSHDR                        ', N'POSHDR                        ', N'CBUSDOCU,CSTYTYPE,PO', N'''P'',''P'',PO', N'PO                                                                                                  ', N'''P'',''P'',.F.                                                                                         ', N'                              ', N'''P'',PO                        ', N'''P'',PO                        ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'32fbb6dd-e07e-4e73-8f33-4fad020e287a', N'ea212aa6-6c7a-49c0-ab76-bec37f2c9547', N'Style PO Shipment             ', 0, N'SHIPNO,CBUSDOCU,CSHPTYPE                                                                            ', N'SHIPNO,''P'',''P''                                                                                      ', N'PO', N'POSHP               ', 1, N'', N'                    ', N'<ExtraAttributes ObjectType="Style PO Shipment">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>SHIPNO</Name>
<Description>Shipment Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Shipment Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>ETA</Name>
<Description>Shipment ETA</Description>
<DataType>Date</DataType>  
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Shipment Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBUSDOCU</Name>
<Description>Business Document Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSHPTYPE</Name>
<Description>Shipment Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTEMPLATE</Name>
<Description>Template Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'ShipNo', N'SHPMTHDR                      ', N'SHPMTHDR                      ', N'CBUSDOCU,CSHPTYPE,SHIPNO', N'''P'',''P'',SHIPNO', N'SHIPNO                                                                                              ', N''''',''P'',''P''                                                                                          ', N'                              ', N'''Q'',''P'',''P'',SHIPNO            ', N'''Q'',SHIPNO            ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'92596a00-e62e-47dd-9934-4c5e07779f62', N'a4910c84-08aa-44c7-9c49-9535bceab155', N'Style PO Cost sheet           ', 0, N'PO                                                                                                  ', N'PO                                                                                                  ', N'PO', N'POCSSH              ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Style PO Cost Sheet">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>PO</Name>
<Description>Purchase Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Vendor</Name>
<Description>Vendor</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Available</Name>
<Description>Purchase Order Available Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Purchase Order Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Complete</Name>
<Description>Purchase Order complete Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Purchase Order Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType> 
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CPURCODE</Name>
<Description>Purchase Group Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBUSDOCU</Name>
<Description>Business Document Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSTYTYPE</Name>
<Description>Style PO Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CLCNO</Name>
<Description>Letter of Credit Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description>
<DataType>VarChar</DataType>
<Width>4</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CMRP</Name>
<Description>MRP Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'PO', N'POSHDR                        ', N'POSHDR                        ', N'CBUSDOCU,CSTYTYPE,PO', N'''P'',''P'',PO', N'PO                                                                                                  ', N'''I'',''?''                                                                                             ', N'                              ', N'                              ',N'''P'',PO')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'6c7fbd01-a64e-46fb-b1f7-e3cbc4b30dcd', N'27d46954-7268-4ec5-b073-9477364af991', N'Cutting Ticket                ', 0, N'PO                                                                                                  ', N'PO                                                                                                  ', N'MF', N'MFCUTKT             ', 1, N'', N'                    ', N'<ExtraAttributes ObjectType="Cutting Ticket">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>PO</Name>
<Description>Cutting Ticket Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Style</Name>
<Description>Style</Description>
<DataType>VarChar</DataType>
<Width>19</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Start</Name>
<Description>Cutting Ticket Start Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Cutting Ticket Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Complete</Name>
<Description>Cutting Ticket complete Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Cutting Ticket Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SEASON</Name>
<Description>Season Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBUSDOCU</Name>
<Description>Business Document Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSTYTYPE</Name>
<Description>Style PO Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CLCNO</Name>
<Description>Letter of Credit Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description>
<DataType>VarChar</DataType>
<Width>4</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CMRP</Name>
<Description>MRP Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'PO', N'POSHDR                        ', N'POSHDR                        ', N'CBUSDOCU,CSTYTYPE,PO', N'''P'',''U'',PO', N'PO                                                                                                  ', N'''''                                                                                                  ', N'                              ', N'''U'',PO                        ',N'''I'',PO')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes] ) VALUES (N'de5751c4-2c21-4c12-9e22-fcc24ba84832', N'95e5408c-c8b3-486c-9c7a-e2dd337a5f3e', N'Cutting Ticket Cost Sheet     ', 0, N'CTRANTYPE,PO                                                                                        ', N'''M'',PO                                                                                              ', N'MF', N'MFCSSH              ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Cutting Ticket Cost Sheet">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>PO</Name>
<Description>Cutting Ticket Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Style</Name>
<Description>Style</Description>
<DataType>VarChar</DataType>
<Width>19</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Start</Name>
<Description>Cutting Ticket Start Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Cutting Ticket Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Complete</Name>
<Description>Cutting Ticket complete Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Cutting Ticket Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SEASON</Name>
<Description>Season Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBUSDOCU</Name>
<Description>Business Document Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSTYTYPE</Name>
<Description>Style PO Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CLCNO</Name>
<Description>Letter of Credit Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description>
<DataType>VarChar</DataType>
<Width>4</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CMRP</Name>
<Description>MRP Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'PO', N'POSHDR                        ', N'POSHDR                        ', N'CBUSDOCU,CSTYTYPE,PO', N'''P'',''U'',PO', N'PO                                                                                                  ', N'''M'',''?''                                                                                             ', N'                              ', NULL,N'''I'',PO')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'119c108e-de35-41dd-a14d-c8721bd8c6a9', N'0751c5df-f457-4cbf-99d8-4af8d1b5abad', N'Manfactured Style Cost Sheet  ', 0, N'CITMMAJOR,CCSTSHT_ID                                                                                ', N'CITMMAJOR,CCSTSHT_ID                                                                                ', N'MF', N'MFSCTSH             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Manfactured Style Cost Sheet">  
<ObjectTypeExtraAttributes> 
<Attribute> 
<Name>CCSTSHT_ID</Name>
<Description>Cost Sheet ID</Description>
<DataType>VarChar</DataType>
<Width>6</Width>  
<Decimals>0</Decimals> 
<ValidEntries/><DefaultValue/> 
<ShowInBrowse>True</ShowInBrowse> 
</Attribute>  
<Attribute><Name>CITMMAJOR</Name>
<Description>Style Major</Description>
<DataType>VarChar</DataType> 
<Width>19</Width> 
<Decimals>0</Decimals> 
<ValidEntries/>
<DefaultValue/>  
<ShowInBrowse>True</ShowInBrowse>  
</Attribute> 
<Attribute> 
<Name>LDEFCSTSHT</Name> 
<Description>Default Cost Sheet</Description>  
<DataType>Bit</DataType>  
<Width>1</Width> 
<Decimals>0</Decimals>  
<ValidEntries/><DefaultValue/>  
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>
<Attribute>
<Name>LBASONSIZ</Name>
<Description>Cost Sheet Based on Sizes</Description>
<DataType>Bit</DataType><Width>1</Width>
<Decimals>0</Decimals><ValidEntries/> 
<DefaultValue/>  
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>cStatus</Name>
<Description>Cost Sheet Status</Description> 
<DataType>VarChar</DataType> 
<Width>1</Width> 
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description> 
<DataType>VarChar</DataType> 
<Width>1</Width> 
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>CCSTSHTTYP</Name>
<Description>Cost Sheet Type</Description> 
<DataType>VarChar</DataType> 
<Width>1</Width> 
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
</ObjectTypeExtraAttributes>  
</ExtraAttributes>', N'CITMMAJOR', N'BOMHEADR                      ', N'BOMHEADR                      ', N'CINVTYPE,CITMMAJOR,CCSTSHTTYP,CCSTSHT_ID', N'''0001'',CITMMAJOR,''M''', N'cItmMajor,CCSTSHT_ID                                                                                ', NULL, N'ccstshtdsc                    ', N'''H'',cItmMajor,CCSTSHT_ID      ',N'''F'',cItmMajor')

INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes] ) VALUES (N'28cc419e-371c-4559-ad2b-a9550977eee1', N'd441512d-06d7-4d5c-ac2e-37ca6e590222', N'Imported Style Cost Sheet     ', 0, N'CITMMAJOR,CCSTSHT_ID                                                                                ', N'CITMMAJOR,CCSTSHT_ID                                                                                ', N'PO', N'POSCTSH             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Imported Style Cost Sheet">  
<ObjectTypeExtraAttributes>  
<Attribute>
<Name>CCSTSHT_ID</Name>
<Description>Cost Sheet ID</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CITMMAJOR</Name>
<Description>Style Major</Description>
<DataType>VarChar</DataType>
<Width>19</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse> 
</Attribute>
<Attribute> 
<Name>LDEFCSTSHT</Name>
<Description>Default Cost Sheet</Description>
<DataType>Bit</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>
<Attribute>
<Name>LBASONSIZ</Name>
<Description>Cost Sheet Based on Sizes</Description>
<DataType>Bit</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse> 
</Attribute>
<Attribute>
<Name>cStatus</Name>
<Description>Cost Sheet Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute> 
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description> 
<DataType>VarChar</DataType> 
<Width>1</Width> 
<Decimals>0</Decimals> 
<ValidEntries/> 
<DefaultValue/> 
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
<Attribute> 
<Name>CCSTSHTTYP</Name>
<Description>Cost Sheet Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse> 
</Attribute> 
</ObjectTypeExtraAttributes></ExtraAttributes>', N'CITMMAJOR', N'BOMHEADR                      ', N'BOMHEADR                      ', N'CINVTYPE,CITMMAJOR,CCSTSHTTYP,CCSTSHT_ID', N'''0001'',CITMMAJOR,''I''', N'cItmMajor,CCSTSHT_ID                                                                                ', NULL, N'ccstshtdsc                    ', N'''H'',cItmMajor,CCSTSHT_ID      ',N'''F'',cItmMajor')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes],[entityRelatedProgs]) VALUES (N'265c14a1-a6d3-43d6-8bf1-d2e678b92df9', N'6bb9c070-0bab-4fe3-a828-35b3e747ab54', N'Invoice                       ', 0, N'INVOICE,ACCOUNT                                                                                     ', N'INVOICE                                                                                             ', N'AR', N'ARDINV              ', 1, N'', N'                    ', N'<ExtraAttributes ObjectType="Invoice">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>INVOICE</Name>
<Description>Invoice Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Order</Name>
<Description>Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>INVDATE</Name>
<Description>Invoice Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPDATE</Name>
<Description>Ship Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DUEDATE</Name>
<Description>Due Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Invoice Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CustPO</Name>
<Description>Customer PO Number</Description>
<DataType>VarChar</DataType>
<Width>15</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Rep1</Name>
<Description>Sales Rep Code 1</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Rep2</Name>
<Description>Sales Rep Code 2</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SEASON</Name>
<Description>Season Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'Invoice', N'INVHDR                        ', N'INVHDR                        ', N'INVOICE', N'Invoice', N'INVOICE                                                                                             ', N'.F.,''?''                                                                                             ', N'                              ', N'''I'',Invoice                   ', N'''C'',Invoice                   ',N'ARIINV,ARVINV')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'17f2629b-7793-4e66-b86f-b8ba04954014', N'2a46da97-0b31-4f9b-9219-68f24a9a9f9e', N'Sales Representative          ', 0, N'REPCODE                                                                                             ', N'REPCODE                                                                                             ', N'SR', N'SRSLSRP             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Sales Representative">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>REPCODE</Name>
<Description>Sales Rep. Code</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>REGION</Name>
<Description>Region Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>NAME</Name>
<Description>Name</Description>
<DataType>VarChar</DataType>
<Width>30</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'repcode', N'SALESREP                      ', N'SALESREP                      ', N'REPCODE', N'REPCODE                                                                                                                 ', N'REPCODE                                                                                             ', NULL, N'name                          ', N'''P'',REPCODE                   ', N'''J'',REPCODE                   ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink]) VALUES (N'11618c5b-42d9-4d25-9a0d-3bf8ab950f06', N'f8ac2a0c-ab5c-44cc-a1eb-cdcbefe9f8e8', N'Payable Invoice               ', 0, N'CVENDCODE,CINVNO                                                                                    ', N'CVENDCODE,CINVNO                                                                                    ', N'AP', N'APPYINV             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Payable Invoice">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>CINVNO</Name>
<Description>Invoice Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CVENDCODE</Name>
<Description>Vendor</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DINVDUDAT</Name>
<Description>Due Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DPOSTDATE</Name>
<Description>Posting Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CVENPMETH</Name>
<Description>Payment Method</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CVENPRIOR</Name>
<Description>Payment Priority</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CCURRCODE</Name>
<Description>Currency Code</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'CINVNO,CVENDCODE', N'APINVHDR                      ', N'INVVEND                       ', N'CINVNO,CVENDCODE', N'CINVNO,CVENDCODE', N'CINVNO,CVENDCODE                                                                                    ', NULL, N'                              ', N'''V'',CINVNO,CVENDCODE          ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'7a8cf1ae-11e5-4f5d-9c91-888e8d65a97a', N'affc9204-d2b5-4eaa-afa6-1a416e9d2c02', N'Vendor                        ', 0, N'CVENDCODE                                                                                           ', N'CVENDCODE                                                                                           ', N'AP', N'APVENDR             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Vendor">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>CVENDCODE</Name>
<Description>Vendor</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CVENCOMP</Name>
<Description>Vendor Company</Description>
<DataType>VarChar</DataType>
<Width>30</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CPHONENO</Name>
<Description>Phone</Description>
<DataType>VarChar</DataType>
<Width>16</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CVENPMETH</Name>
<Description>Payment Method</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CVENPRIOR</Name>
<Description>Payment Priority</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CCURRCODE</Name>
<Description>Currency Code</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'cVendCode', N'APVENDOR                      ', N'VENCODE                       ', N'CVENDCODE', N'CVENDCODE                                                                                                               ', N'CVENDCODE                                                                                           ', NULL, N'cvencomp                      ', N'                              ', N'''H'',CVENDCODE ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'88480cb0-53b9-476e-bb0c-1e1960e66a85', N'efdefd11-8886-4db4-9b21-c636e915e5a9', N'Credit Memo                   ', 0, N'CRMEMO,ACCOUNT                                                                                      ', N'CRMEMO                                                                                              ', N'RM', N'RMCRMEM             ', 1, N'', N'RMCMEM              ', N'<ExtraAttributes ObjectType="Credit Memo">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>CRMEMO</Name>
<Description>Credit Memo Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Order</Name>
<Description>Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CRDATE</Name>
<Description>Credit Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>INVOICE</Name>
<Description>Invoice Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DPOSTDATE</Name>
<Description>Posting Date</Description>
<DataType>Date</DataType>
<Width>8</Width>  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>RANO</Name>
<Description>Return Auth. Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SALESREP1</Name>
<Description>Sales Rep Code 1</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SALESREP2</Name>
<Description>Sales Rep Code 2</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>REASON</Name>
<Description>Return Reason Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'CRMEMO', N'RETHDR                        ', N'RETHDR                        ', N'CRMEMO', N'CRMEMO                        ', N'CRMEMO                                                                                              ', N'''''                                                                                                  ', N'                              ', N'''D'',CRMEMO                    ', N'''R'',CRMEMO                    ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes]) VALUES (N'4203b19b-d213-40d3-b80e-bf04b7cd9737', N'41ed399b-5775-4b26-a05e-86816df936f7', N'Return Authorization          ', 0, N'RANO,ACCOUNT                                                                                        ', N'RANO                                                                                                ', N'RM', N'RMRTATH             ', 1, N'', N'RMRAUT              ', N'<ExtraAttributes ObjectType="Return Authorization">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>RANO</Name>
<Description>Return Auth. Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Order</Name>
<Description>Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>radate</Name>
<Description>Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>INVOICE</Name>
<Description>Invoice Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>REASON</Name>
<Description>Return Reason Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CCURRCODE</Name>
<Description>Currency Code</Description>
<DataType>VarChar</DataType>
<Width>3</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'RANO', N'RETAUTH                       ', N'RETAUTH                       ', N'RANO', N'RANO', N'RANO                                                                                                ', N'''''                                                                                                  ', N'                              ', N'                              ', N'''Z'',RANO')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes] ) VALUES (N'ffb3e7f1-04fb-44ee-a727-4c8c9f7dff35', N'8a25c4f9-e847-469b-9c21-ca66018350c6', N'Material Purchase Order       ', 0, N'CBUSDOCU,CSTYTYPE,PO                                                                                ', N'''P'',''M'',PO                                                                                          ', N'MA', N'MAPRCAM             ', 1, N'', N'MAMATP              ', N'<ExtraAttributes ObjectType="Material Purchase Order">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>PO</Name>
<Description>Purchase Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Vendor</Name>
<Description>Vendor</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Available</Name>
<Description>Purchase Order Available Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Purchase Order Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Complete</Name>
<Description>Purchase Order complete Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Purchase Order Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CDIVISION</Name>
<Description>Division Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTERMCODE</Name>
<Description>Payment Terms Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>  
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CPURCODE</Name>
<Description>Purchase Group Code</Description>
<DataType>VarChar</DataType>
<Decimals>0</Decimals>
<Width>6</Width>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBUSDOCU</Name>
<Description>Business Document Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSTYTYPE</Name>
<Description>Style PO Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CLCNO</Name>
<Description>Letter of Credit Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CINVTYPE</Name>
<Description>Inventory Type</Description>
<DataType>VarChar</DataType>
<Width>4</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CMRP</Name>
<Description>MRP Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'PO', N'POSHDR                        ', N'POSHDR                        ', N'CBUSDOCU,CSTYTYPE,PO', N'''P'',''M'',PO', N'PO                                                                                                  ', N'''P'',''M'',.F.                                                                                         ', N'                              ', N'''M'',PO                        ', N'''M'',''P'',PO                        ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityNotes] ) VALUES (N'1dcd3861-0b0c-4852-b173-fdd58b8a0528', N'8b661d81-0644-44af-81e6-ed83ab4071eb', N'Material PO Shipment          ', 0, N'SHIPNO                                                                                              ', N'SHIPNO                                                                                              ', N'MA', N'MASHP               ', 1, N'', N'                    ', N'<ExtraAttributes ObjectType="Material PO Shipment">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>SHIPNO</Name>
<Description>Shipment Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Entered</Name>
<Description>Shipment Entered Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>ETA</Name>
<Description>Shipment ETA</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Shipment Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBUSDOCU</Name>
<Description>Business Document Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSHPTYPE</Name>
<Description>Shipment Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CTEMPLATE</Name>
<Description>Template Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'ShipNo', N'SHPMTHDR                      ', N'SHPMTHDR                      ', N'CBUSDOCU,CSHPTYPE,SHIPNO', N'''P'',''M'',SHIPNO', N'SHIPNO                                                                                              ', N'''''                                                                                                  ', N'                              ', N'''Q'',''P'',''M'',SHIPNO            ', N'''Q'',SHIPNO            ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink]) VALUES (N'559c0614-495b-4014-bd74-d2a93ec8510e', N'dae0bde8-3fe2-4898-9104-de1313947ad8', N'Packing List                  ', 0, N'PACK_NO                                                                                             ', N'PACK_NO                                                                                             ', N'AL', N'ALPLIST             ', 1, N'', N'ALPKLS              ', N'<ExtraAttributes ObjectType="Packing List">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>PACK_NO</Name>
<Description>Pack Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIP_DATE</Name>
<Description>Ship Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>ORDER</Name>
<Description>Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>BILL_LADG</Name>
<Description>BillOf Lading Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'pack_no', N'PACK_HDR                      ', N'PACK_HDR                      ', N'PACK_NO', N'PACK_NO', N'PACK_NO                                                                                             ', N'''''                                                                                                  ', N'                              ', N'''L'',PACK_NO                   ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink],[entityRelatedProgs]) VALUES (N'262c34de-b5ec-4933-b5db-a5088082a449', N'b68d46f0-43ae-4c22-8e9c-53307433b7ef', N'Picking Ticket                ', 0, N'PIKTKT                                                                                              ', N'PIKTKT                                                                                              ', N'AL', N'ALPKTKT             ', 0, N'', N'                    ', N'<ExtraAttributes ObjectType="Picking Ticket">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>PIKTKT</Name>
<Description>Picking Ticket Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DATE</Name>
<Description>Picking Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>ORDER</Name>
<Description>Order Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CWARECODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CustPO</Name>
<Description>Customer PO Number</Description>
<DataType>VarChar</DataType>
<Width>15</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'piktkt', N'PIKTKT                        ', N'PIKTKT                        ', N'PIKTKT', N'PIKTKT', N'PIKTKT                                                                                              ', N'                                                                                                    ', N'                              ', N'''K'',PIKTKT                    ',N'ALAUTAL,ALSTYAL,ALORDAL')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink]) VALUES (N'0b978a7d-614e-4ef1-ba87-ace12e74d65a', N'22dff895-c2c0-43b0-9a10-ce2f38fb6307', N'Bill of Lading                ', 0, N'BOL_NO                                                                                              ', N'BOL_NO                                                                                              ', N'AL', N'ALBOL               ', 1, N'', N'ALPBOL              ', N'<ExtraAttributes ObjectType="Bill of Lading">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>BOL_NO</Name>
<Description>Bill Of Lading Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Account</Name>
<Description>Account</Description>
<DataType>VarChar</DataType>
<Width>5</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>SHIP_DATE</Name>
<Description>Ship Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Status</Name>
<Description>Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>Store</Name>
<Description>Customer Store Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>W_CODE</Name>
<Description>Warehouse Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute> 
<Attribute>
<Name>SHIPVIA</Name>
<Description>Ship Via Code</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'BOL_NO', N'BOL_HDR                       ', N'BOL_HDR                       ', N'BOL_NO', N'BOL_NO', N'BOL_NO                                                                                              ', N'''''                                                                                                  ', N'                              ', N'                              ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink]) VALUES (N'11604f51-1aa5-4ea9-99c2-01dfe8fdf09f', N'21d41fb5-2120-4652-bec7-59f0c9c01646', N'Journal Batch                 ', 0, N'CBATCHNO                                                                                            ', N'CBATCHNO                                                                                            ', N'GL', N'GLBATCH             ', 1, N'', N'                    ', N'<ExtraAttributes ObjectType="Journal Batch">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>CBATCHNO</Name>
<Description>Batch Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>   
<Attribute>
<Name>CBATSTAT</Name>
<Description>Batch Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>LBATIND</Name>
<Description>Batch indicator</Description>
<DataType>Bit</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBATTYPE</Name>
<Description>Batch Type</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DBATPBEG</Name>
<Description>Beginning Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DBATPEND</Name>
<Description>End Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'cbatchno', N'GLBATCH                       ', N'BATCHNO                       ', N'CBATCHNO', N'CBATCHNO                                                                                                                ', N'CBATCHNO                                                                                            ', N'''''                                                                                                  ', N'                              ', N'                              ')
INSERT [dbo].[EntityTypeSettings] ([oid], [entitytypeoid], [entityname], [uselivetile], [keyexpression], [keyvalue], [moduleid], [viewinterface], [addnew], [addinterface], [relatedreportname], [extradata], [searchfield], [mastertable], [mastertableindex], [mastertableindexexpression], [indexexpressiondefaultvalue], [recentfields], [newmodeparameters], [entitydescfield], [entityobjlink]) VALUES (N'e37d6015-021a-42f9-889f-9a17b7e47504', N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8', N'GL Single Transaction         ', 0, N'CBATCHNO,CTRANNO                                                                                    ', N'''000000'',CTRANNO                                                                                    ', N'GL', N'GLTRANS             ', 1, N'', N'                    ', N'<ExtraAttributes ObjectType="GL Single Transaction">
<ObjectTypeExtraAttributes>
<Attribute>
<Name>CTRANNO</Name>
<Description>GL Transacion Number</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>   
<Attribute>
<Name>CTRNSTAT</Name>
<Description>Transaction Status</Description>
<DataType>VarChar</DataType>
<Width>1</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>True</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CBATCHNO</Name>
<Description>Batch Number</Description>
<DataType>VarChar</DataType>
<Width>6</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DTRNPDATE</Name>
<Description>Transaction Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>DTRNREVDT</Name>
<Description>Reversing Date</Description>
<DataType>Date</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CSRCJRNL</Name>
<Description>Source Journal</Description>
<DataType>VarChar</DataType>
<Width>2</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
<Attribute>
<Name>CAUTCODE</Name>
<Description>Template Code</Description>
<DataType>VarChar</DataType>
<Width>8</Width>
<Decimals>0</Decimals>
<ValidEntries/>
<DefaultValue/>
<ShowInBrowse>False</ShowInBrowse>
</Attribute>
</ObjectTypeExtraAttributes>
</ExtraAttributes>', N'ctranno', N'GLTRNSHD                      ', N'BATCHTRN                      ', N'CBATCHNO,CTRANNO', N'''000000'',CTRANNO', N'CTRANNO                                                                                             ', N''''',.F.                                                                                              ', N'                              ', N'                              ')

INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'd95d4e8f-d36f-492e-a9ca-0170d2630c23', N'PACKED                        ', N'Packed                                                                                              ', N'b68d46f0-43ae-4c22-8e9c-53307433b7ef')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'f2d5332a-64b6-4638-9d34-09b6c1888b2a', N'ACTIVE                        ', N'A - Active                                                                                          ', N'0751c5df-f457-4cbf-99d8-4af8d1b5abad')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'15c09103-9b97-4dcf-807a-1039ae923cb5', N'COMPLETE                      ', N'Complete                                                                                            ', N'2c37932b-24f8-4ba2-84dc-6fe41acd5751')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'fc1d21be-fc9e-4b42-bae8-12e03f1a7954', N'COMPLETE                      ', N'Complete                                                                                            ', N'41aa07fe-f779-4850-a9ab-d991c2c80a32')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'e67f5b19-e7a2-4d55-9b23-160ad7c81c7d', N'CANCELED                      ', N'Canceled                                                                                            ', N'affc9204-d2b5-4eaa-afa6-1a416e9d2c02')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'8adf68a2-12bc-4e94-97d4-17761d278ae5', N'CANCELED                      ', N'Canceled                                                                                            ', N'27d46954-7268-4ec5-b073-9477364af991')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'bb267bf5-e7f9-4ee9-8c5e-1852a1cad6b8', N'ACTIVE                        ', N'Active                                                                                              ', N'98fe78bc-2535-49d9-ac1b-326baa16b4ba')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'f932da6e-7cf7-4aba-9b14-2a3c9ec6d318', N'OPEN                          ', N'Open                                                                                                ', N'8b661d81-0644-44af-81e6-ed83ab4071eb')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'64b1e8a1-3b3b-48ec-8710-2a8594c90ec8', N'OPEN                          ', N'Open                                                                                                ', N'41ed399b-5775-4b26-a05e-86816df936f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'fdbccf56-9e39-404c-930a-2a9cbf63bf6b', N'H - HOLD                      ', N'Hold                                                                                                ', N'2c37932b-24f8-4ba2-84dc-6fe41acd5751')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'7a49cf70-df81-497f-8f76-2b1636319c88', N'E - ELECTRONIC                ', N'Electronic                                                                                          ', N'41ed399b-5775-4b26-a05e-86816df936f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'5f6abf78-2535-4693-8d4e-2b426958a1d0', N'H - HOLD                      ', N'Hold                                                                                                ', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'e6ba0c66-21af-4ecd-97d2-2e918583507a', N'O - OPEN                      ', N'Open                                                                                                ', N'ea212aa6-6c7a-49c0-ab76-bec37f2c9547')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ac9368a2-6a3f-4bf4-9ceb-370464ff5a52', N'O - OPEN                      ', N'Open                                                                                                ', N'6bb9c070-0bab-4fe3-a828-35b3e747ab54')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'38df82cd-3899-43c0-8a83-3727e0830074', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'c1a30d03-b29e-4c7f-bd18-37379c796144', N'H - HOLD                      ', N'Hold                                                                                                ', N'83fadaa5-4b9a-4140-9798-d89d78cf48f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'29e5af2f-fb6f-40d2-87fd-38a9b0312995', N'H - HOLD                      ', N'Hold                                                                                                ', N'1e5b9214-8771-44fb-a6c3-fbf0f101d4f8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'a1eadd6b-a1a1-4122-8cd0-3a0991af7a3a', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'41ed399b-5775-4b26-a05e-86816df936f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'310e15dd-b54d-43aa-8a34-3c7f697e47f6', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'ea212aa6-6c7a-49c0-ab76-bec37f2c9547')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'06f7e78e-f63b-4d3f-b170-3cf79f6249e2', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'27d46954-7268-4ec5-b073-9477364af991')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'2e3de043-569c-4cae-8113-3ddd3754d405', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'6bb9c070-0bab-4fe3-a828-35b3e747ab54')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'869e6177-fe23-4c69-9f59-3eda454066bf', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'b68d46f0-43ae-4c22-8e9c-53307433b7ef')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'edb02424-b1af-424b-82e8-41cdbe455f30', N'P - POSTED                    ', N'Posted                                                                                              ', N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ced4b4dc-1768-42d9-918c-43e6d399a159', N'P - PULLED                    ', N'Pulled                                                                                              ', N'b68d46f0-43ae-4c22-8e9c-53307433b7ef')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'402098aa-b854-44c9-87c1-4cf5bfaee048', N'A - ACTIVE                    ', N'Active                                                                                              ', N'affc9204-d2b5-4eaa-afa6-1a416e9d2c02')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'3f452fce-6e72-4647-b340-4ee6aa4d20c4', N'S - CLOSED                    ', N'Closed                                                                                              ', N'27d46954-7268-4ec5-b073-9477364af991')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'529f4436-1e8c-4e45-9a97-4f219a6fe070', N'H - HOLD                      ', N'Hold                                                                                                ', N'21d41fb5-2120-4652-bec7-59f0c9c01646')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'f842da27-cd44-47c6-a6d2-50444beba20a', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'ea212aa6-6c7a-49c0-ab76-bec37f2c9547')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'3d83062b-98f2-4337-8163-52677d0165cc', N'A - ACTIVE                    ', N'Active                                                                                              ', N'83fadaa5-4b9a-4140-9798-d89d78cf48f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'284f25ff-bdac-4e76-91cf-5cfe6325cf85', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'4ae1c60c-367f-4679-a59d-63b7e1a9fd86', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'1e5b9214-8771-44fb-a6c3-fbf0f101d4f8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'f50c39e9-5678-4f4d-ad25-63f72c4de16d', N'A - ACTIVE                    ', N'Active                                                                                              ', N'd441512d-06d7-4d5c-ac2e-37ca6e590222')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'7215978c-0e72-4782-98db-6432acca9645', N'P - POTENTIAL                 ', N'Potential                                                                                           ', N'83fadaa5-4b9a-4140-9798-d89d78cf48f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'0dfb85d2-fc76-416e-8009-64d5c7087baa', N'B - BID                       ', N'Bid                                                                                                 ', N'27d46954-7268-4ec5-b073-9477364af991')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'83c622f5-ad97-4875-bc5d-65ad76fc1072', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'8b661d81-0644-44af-81e6-ed83ab4071eb')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'15622681-b390-4ca3-b25e-66cf084ca7e4', N'  - OPEN                      ', N'Open                                                                                                ', N'dae0bde8-3fe2-4898-9104-de1313947ad8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'6fb11eb8-6409-465e-81ed-6736090dad93', N'O - OPEN                      ', N'Open                                                                                                ', N'b68d46f0-43ae-4c22-8e9c-53307433b7ef')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'3bd567e4-e084-4fe9-91da-6c7aaad00efe', N'S - SUMMARIZED                ', N'Summarized                                                                                          ', N'21d41fb5-2120-4652-bec7-59f0c9c01646')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'c650e464-190a-4953-b302-71dacdd41fdf', N'B - BID                       ', N'Bid                                                                                                 ', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'48bcbd33-fc21-47eb-a6ce-74fac87e33f0', N'E - EMPTY                     ', N'Empty                                                                                               ', N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ce90048a-3a0d-476c-b1ca-7592c7f42bc6', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'41ed399b-5775-4b26-a05e-86816df936f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'dc9e8ba8-e4d1-403d-8560-77e1a337bc0d', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'0751c5df-f457-4cbf-99d8-4af8d1b5abad')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'd6cf4f08-4b4c-4861-95dc-80dfe1449600', N'P - POSTED                    ', N'Posted                                                                                              ', N'21d41fb5-2120-4652-bec7-59f0c9c01646')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'0df8fab9-4bff-40b9-a394-813e1376f13b', N'C - SHIPPED                   ', N'Shipped                                                                                             ', N'dae0bde8-3fe2-4898-9104-de1313947ad8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'7a10adaf-9da4-41f2-bb74-843d530b0f9f', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'7beabeb3-ff3f-481f-b471-8595389ddbf6', N'V - VOID                      ', N'Void                                                                                                ', N'6bb9c070-0bab-4fe3-a828-35b3e747ab54')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'd6e3b7ba-d159-48ca-ad6a-85da9a00ed69', N'H - HOLD                      ', N'Hold                                                                                                ', N'0751c5df-f457-4cbf-99d8-4af8d1b5abad')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'c5cdae00-9380-45bd-b72f-85ec3751888b', N'I - INWORK                    ', N'In Work                                                                                             ', N'0751c5df-f457-4cbf-99d8-4af8d1b5abad')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'fee51381-733c-423b-ace5-8ea48cc18814', N'H - HOLD                      ', N'Hold                                                                                                ', N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'2ca42b4a-2341-46c7-aa56-91f9986692c6', N'O - OUT OF BALANCE            ', N'Out of Balance                                                                                      ', N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'abf5b3e2-8eee-4a78-afda-95a0afafb42a', N'A - ACTUALIZED                ', N'Actualized                                                                                          ', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'4f149894-2c6f-42cf-87e8-9644d9f9b987', N'O - OPEN                      ', N'Open                                                                                                ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'd83156e1-967a-4442-b38f-9897783c2c51', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'98fe78bc-2535-49d9-ac1b-326baa16b4ba')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'90396bc8-bd00-4f22-a613-990a40746733', N'  - OPEN                      ', N'Open                                                                                                ', N'f8ac2a0c-ab5c-44cc-a1eb-cdcbefe9f8e8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ac9ac45f-2eb7-41eb-8333-9ab83078d166', N'H -  HOLD                     ', N'Hold                                                                                                ', N'98fe78bc-2535-49d9-ac1b-326baa16b4ba')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ea0bb6a5-e7a6-4a74-8c8b-9c84dfb023ff', N'V - VOID                      ', N'Void                                                                                                ', N'efdefd11-8886-4db4-9b21-c636e915e5a9')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'b73555df-04c8-4164-a6bf-9e8e686788c0', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'52e61589-3b75-4f18-8d77-9fccbc72c778', N'O - OUT OF BALANCE            ', N'Out of Balance                                                                                      ', N'21d41fb5-2120-4652-bec7-59f0c9c01646')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'a348a879-930f-4437-bd1e-9fd00aa64699', N'  - ACTIVE                    ', N'Active                                                                                              ', N'efdefd11-8886-4db4-9b21-c636e915e5a9')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'4a3194f3-9325-42c2-a2f1-a11f655aabd1', N'U - UNPOSTED                  ', N'Unposted                                                                                            ', N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'bc07c528-a194-4386-94da-a2e563ada94e', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'8b661d81-0644-44af-81e6-ed83ab4071eb')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ba9b063e-7a8d-46f7-8f16-a38a885453de', N'A - ACTUALIZED                ', N'Actualized                                                                                          ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'3b1e8756-f711-4ef3-a79b-a4ec893bf5f6', N'H - HOLD                      ', N'Hold                                                                                                ', N'b68d46f0-43ae-4c22-8e9c-53307433b7ef')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'37d2de41-ceca-4cf6-af98-a64dc6116d07', N'B - BID                       ', N'Bid                                                                                                 ', N'2c37932b-24f8-4ba2-84dc-6fe41acd5751')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'e7bafcdb-1e52-4ada-a7fa-a85afd990663', N'H - HOLD                      ', N'Hold                                                                                                ', N'41aa07fe-f779-4850-a9ab-d991c2c80a32')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'40eca66b-c4f9-4c11-8954-aa0416f6f1f3', N'S - CLOSED                    ', N'Closed                                                                                              ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'05fd8f1d-363e-4657-86ee-ab794608f68e', N'E - EMPTY                     ', N'Empty                                                                                               ', N'21d41fb5-2120-4652-bec7-59f0c9c01646')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'a7f2c6eb-3256-4aa2-822f-ac0e7de4de54', N'H - HOLD                      ', N'Hold                                                                                                ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'042a76a9-303e-48bf-b82d-ac7f12113ec8', N'B - BID                       ', N'Bid                                                                                                 ', N'41aa07fe-f779-4850-a9ab-d991c2c80a32')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'9fef03fb-70f4-44f6-afaf-aff4ce828e9b', N'A - ACTUALIZED                ', N'Actualized                                                                                          ', N'27d46954-7268-4ec5-b073-9477364af991')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'24680546-e6db-4039-b8d8-b324853862e4', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'83fadaa5-4b9a-4140-9798-d89d78cf48f7')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'd99362e2-a55f-4635-9492-b324b4e1f12c', N'  - OPEN                      ', N'Open                                                                                                ', N'22dff895-c2c0-43b0-9a10-ce2f38fb6307')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'c106c1b5-b84a-494e-82d3-b360413de62a', N'A - APPROVED                  ', N'Approved                                                                                            ', N'71e2faf0-d49f-4e61-afe1-b6ee577a5eb8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'8988bdf1-e979-45d8-9f08-b421dfea7049', N'A - ACTIVE                    ', N'Active                                                                                              ', N'1e5b9214-8771-44fb-a6c3-fbf0f101d4f8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'4b7066e3-7a51-47ea-a59f-b8a3282b731f', N'U - UNPOSTED                  ', N'Unposted                                                                                            ', N'21d41fb5-2120-4652-bec7-59f0c9c01646')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'1c66cc36-7574-4a88-9e83-c7386bbc0443', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'41aa07fe-f779-4850-a9ab-d991c2c80a32')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'bfee488c-4778-4985-9255-ca1db5f5d422', N'O - OPEN                      ', N'Open                                                                                                ', N'41aa07fe-f779-4850-a9ab-d991c2c80a32')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ce2e882b-0661-4a6c-8a56-cbe07a2c9ca3', N'O - OPEN                      ', N'Open                                                                                                ', N'2c37932b-24f8-4ba2-84dc-6fe41acd5751')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'a6a9135b-b9ad-47c9-b4a3-ccd1dd5929e3', N'A - APPROVED                  ', N'Approved                                                                                            ', N'21d41fb5-2120-4652-bec7-59f0c9c01646')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'677fecb6-9568-440a-99be-d06d5990b32b', N'S - CLOSED                    ', N'Closed                                                                                              ', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'4275d0f5-cecc-49c8-a393-d14be0a10911', N'O - OPEN                      ', N'Open                                                                                                ', N'27d46954-7268-4ec5-b073-9477364af991')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'ff880da9-1e08-4195-9ea0-d2da13ec0a88', N'I - IN WORK                   ', N'In Work                                                                                             ', N'd441512d-06d7-4d5c-ac2e-37ca6e590222')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'0a8655f0-a48a-40b8-8d42-d5559e790292', N'H - HOLD                      ', N'Hold                                                                                                ', N'affc9204-d2b5-4eaa-afa6-1a416e9d2c02')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'4bf8f15b-0ac1-4d28-a3b0-dbe2e9ebab0e', N'B - BID                       ', N'Bid                                                                                                 ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'c3c5c174-c781-40c2-a439-defe755743a2', N'H - HOLD                      ', N'Hold                                                                                                ', N'd441512d-06d7-4d5c-ac2e-37ca6e590222')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'a4744da9-1bd9-4a00-b566-e214637f5583', N'V - VOID                      ', N'Void                                                                                                ', N'f8ac2a0c-ab5c-44cc-a1eb-cdcbefe9f8e8')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'bbc6f97a-69c8-4d26-a375-e7490f5c1070', N'O - OPEN                      ', N'Open                                                                                                ', N'5fa0eaec-da5c-48e4-b1c5-859f47b6bd30')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'da4061f7-cb34-48bf-a9d6-ece6dcc43619', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'd441512d-06d7-4d5c-ac2e-37ca6e590222')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'6c01f5cf-d70c-4e66-98c3-f2aef8f0250a', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'b68d46f0-43ae-4c22-8e9c-53307433b7ef')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'64740bc7-f2a9-480d-bd9b-f68e24af529b', N'H - HOLD                      ', N'Hold                                                                                                ', N'27d46954-7268-4ec5-b073-9477364af991')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'2fba5ec9-450a-45bf-9b06-fbb5879fb128', N'C - SHIPPED                   ', N'Shipped                                                                                             ', N'22dff895-c2c0-43b0-9a10-ce2f38fb6307')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'd2eb43a1-0fb2-4b5d-aa1b-fc34cc0fdd15', N'X - CANCELED                  ', N'Canceled                                                                                            ', N'2c37932b-24f8-4ba2-84dc-6fe41acd5751')
INSERT [dbo].[EntityStatus] ([oid], [ID], [description], [EntityType]) VALUES (N'4963c85d-f992-4267-a4f8-fe2a95aa9fae', N'C - COMPLETE                  ', N'Complete                                                                                            ', N'8a25c4f9-e847-469b-9c21-ca66018350c6')
