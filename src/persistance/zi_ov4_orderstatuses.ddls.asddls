@AbapCatalog.sqlViewName: 'ZIOV4ORDERSTATUS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order statuses'
@ObjectModel: { compositionRoot: true,
                representativeKey: 'Id',
                semanticKey: ['_Texts.Name'] }
define view ZI_OV4_OrderStatuses
  as select from zov4orderstatus
  association [0..*] to ZI_OV4_OrderStatusTexts as _Texts
    on $projection.Id = _Texts.Id
  association [1..1] to ZI_OV4_OrderStatusLocalized as _LocalizedText
    on $projection.Id = _LocalizedText.Id
  //  association [0..1] to I_User as _CreationUser
  //    on $projection.CreatedBy = _CreationUser.UserID
  //  association [0..1] to I_User as _ModificationUser
  //    on $projection.ModifiedBy = _ModificationUser.UserID
  {
    @ObjectModel.text.association: '_Texts'
    key id as Id,
    @Semantics.systemDate: { createdAt: true }
    created_at as CreatedAt,
    @Semantics.user: { createdBy: true }
    created_by as CreatedBy,
    @Semantics.systemDate: { lastChangedAt: true }
    modified_at as ModifiedAt,
    @Semantics.user: { lastChangedBy: true }
    modified_by as ModifiedBy,
    @ObjectModel: { association: { type: [ #TO_COMPOSITION_CHILD] } }
    _Texts,
    _LocalizedText
    //    _CreationUser,
    //    _ModificationUser
  }
