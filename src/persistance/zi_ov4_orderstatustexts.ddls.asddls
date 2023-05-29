@AbapCatalog.sqlViewName: 'ZIOV4ORDERSTATT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order status texts'
//define view entity ZI_OV4_OrderStatusTexts
define view ZI_OV4_OrderStatusTexts
  as select from zov4orderstatust
  //  association to parent ZI_OV4_OrderStatuses as _Status
  //    on $projection.Id = _Status.Id
  association [1..1] to ZI_OV4_OrderStatuses as _Status
    on $projection.Id = _Status.Id
  association [0..1] to ZI_OV4_OrderStatusLocalized as _LocalizedText
    on $projection.Id = _LocalizedText.Id
  //  association [0..1] to I_User as _CreationUser
  //    on $projection.CreatedBy = _CreationUser.UserID
  //  association [0..1] to I_User as _ModificationUser
  //    on $projection.ModifiedBy = _ModificationUser.UserID
  {
    key language as Language,
    key id as Id,
    created_at as CreatedAt,
    created_by as CreatedBy,
    modified_at as ModifiedAt,
    modified_by as ModifiedBy,
    name as Name,
    description as Description,
    _Status,
    _LocalizedText
    //    _CreationUser,
    //    _ModificationUser
  }
