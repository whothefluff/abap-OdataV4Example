@AbapCatalog.sqlViewName: 'ZIOV4ORDERSTATL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order status texts'
//define view entity ZI_OV4_OrderStatusLocalized
define view ZI_OV4_OrderStatusLocalized
  as select from zov4orderstatus
    left outer join zov4orderstatust
      on zov4orderstatus.id = zov4orderstatust.id
      and zov4orderstatust.language = $session.system_language
  //  association to parent ZI_OV4_OrderStatuses as _Status // only in 754 and after
  //    on $projection.Id = _Status.Id
  association [1..*] to ZI_OV4_OrderStatuses as _Status
    on $projection.Id = _Status.Id
  association [1..*] to ZI_OV4_OrderStatusTexts as _Texts
    on $projection.Id = _Texts.Id
  //  association [0..1] to I_User as _CreationUser
  //    on $projection.CreatedBy = _CreationUser.UserID
  //  association [0..1] to I_User as _ModificationUser
  //    on $projection.ModifiedBy = _ModificationUser.UserID
  {
    key zov4orderstatus.id as Id,
    zov4orderstatust.created_at as CreatedAt,
    zov4orderstatust.created_by as CreatedBy,
    zov4orderstatust.modified_at as ModifiedAt,
    zov4orderstatust.modified_by as ModifiedBy,
    coalesce( zov4orderstatust.name, zov4orderstatus.name ) as Name,
    coalesce( zov4orderstatust.description, zov4orderstatus.description ) as Description,
    _Status,
    _Texts
    //    _CreationUser,
    //    _ModificationUser
  }
