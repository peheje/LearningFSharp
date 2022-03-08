open Tools
open Models.Customer
open Services

let customer = getService<ICustomer> ()

log (serialize customer)

log (customer.Legal())

log (getInstancesCount ())