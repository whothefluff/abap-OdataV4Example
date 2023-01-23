@AbapCatalog.sqlViewName: 'ZIOV4ORDERS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Orders'
define root view ZI_OV4_Orders
  as select from zov4order
  association [0..*] to ZI_OV4_OrderItems as _Items
    on $projection.Id = _Items.UpId
  association [1..1] to ZI_OV4_OrderStatuses as _Status
    on $projection.Status = _Status.Id
  association [1..1] to ZI_OV4_OrderStatusLocalized as _LocalizedStatus
    on $projection.Status = _LocalizedStatus.Id
  association [0..1] to I_User as _CreationUser
    on $projection.CreatedBy = _CreationUser.UserID
  association [0..1] to I_User as _ModificationUser
    on $projection.ModifiedBy = _ModificationUser.UserID
  {
    key id as Id,
    external_id as ExternalId,
    created_at as CreatedAt,
    created_by as CreatedBy,
    modified_at as ModifiedAt,
    modified_by as ModifiedBy,
    status as Status,
    _Items,
    _Status,
    _LocalizedStatus,
    _CreationUser,
    _ModificationUser
  }
