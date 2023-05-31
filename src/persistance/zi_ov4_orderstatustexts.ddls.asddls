@AbapCatalog.sqlViewName: 'ZIOV4ORDERSTATT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order status texts'
@ObjectModel: { dataCategory: #TEXT,
                representativeKey: 'Id',
                semanticKey: ['Name'] }
define view ZI_OV4_OrderStatusTexts
  as select from zov4orderstatust
  association [1..1] to ZI_OV4_OrderStatuses as _Status
    on $projection.Id = _Status.Id
  association [1..1] to ZI_OV4_OrderStatusLocalized as _LocalizedText
    on $projection.Id = _LocalizedText.Id
  //  association [0..1] to I_User as _CreationUser
  //    on $projection.CreatedBy = _CreationUser.UserID
  //  association [0..1] to I_User as _ModificationUser
  //    on $projection.ModifiedBy = _ModificationUser.UserID
  {
    @Semantics.language: true
    key language as Language,
    key id as Id,
    @Semantics.systemDate: { createdAt: true }
    created_at as CreatedAt,
    @Semantics.user: { createdBy: true }
    created_by as CreatedBy,
    @Semantics.systemDate: { lastChangedAt: true }
    modified_at as ModifiedAt,
    @Semantics.user: { lastChangedBy: true }
    modified_by as ModifiedBy,
    @Semantics.text: true
    name as Name,
    @Semantics.text: true
    description as Description,
    @ObjectModel.association: { type: [ #TO_COMPOSITION_PARENT,
                                        #TO_COMPOSITION_ROOT ] }
    _Status,
    _LocalizedText
    //    _CreationUser,
    //    _ModificationUser
  }
