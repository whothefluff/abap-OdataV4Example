@AbapCatalog.sqlViewName: 'ZIOV4ORDERSTATUS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order statuses'
//define root view entity ZI_OV4_OrderStatuses
define view ZI_OV4_OrderStatuses
  as select from zov4orderstatus
  //  composition [0..*] of ZI_OV4_OrderStatusTexts as _Texts
  association [0..*] to ZI_OV4_OrderStatusTexts as _Texts
    on $projection.Id = _Texts.Id
  //  composition [0..1] of ZI_OV4_OrderStatusLocalized as _LocalizedText
  association [0..*] to ZI_OV4_OrderStatusLocalized as _LocalizedText
    on $projection.Id = _LocalizedText.Id
  //  association [0..1] to I_User as _CreationUser
  //    on $projection.CreatedBy = _CreationUser.UserID
  //  association [0..1] to I_User as _ModificationUser
  //    on $projection.ModifiedBy = _ModificationUser.UserID
  {
    key id as Id,
    created_at as CreatedAt,
    created_by as CreatedBy,
    modified_at as ModifiedAt,
    modified_by as ModifiedBy,
    _Texts,
    _LocalizedText
    //    _CreationUser,
    //    _ModificationUser
  }
