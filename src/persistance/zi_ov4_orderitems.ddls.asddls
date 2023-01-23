@AbapCatalog.sqlViewName: 'ZIOV4ORDERITEMS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order items'
define view ZI_OV4_OrderItems
  as select from zov4orderitem
  association [1..1] to ZI_OV4_Orders as _Header
    on $projection.UpId = _Header.Id
  association [1..1] to I_UnitOfMeasure as _UnitOfMeasure
    on $projection.Unit = _UnitOfMeasure.UnitOfMeasure
  association [0..1] to I_User as _CreationUser
    on $projection.CreatedBy = _CreationUser.UserID
  association [0..1] to I_User as _ModificationUser
    on $projection.ModifiedBy = _ModificationUser.UserID
  {
    key zov4orderitem.up_id as UpId,
    key id as Id,
    created_at as CreatedAt,
    created_by as CreatedBy,
    modified_at as ModifiedAt,
    modified_by as ModifiedBy,
    unit as Unit,
    quantity as Quantity,
    _Header,
    _UnitOfMeasure,
    _CreationUser,
    _ModificationUser
  }
