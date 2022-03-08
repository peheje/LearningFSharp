open Models.Customer
open Services
open Tools

let customer = getService<ICustomer> ()

log (serialize customer)

log (customer.Legal())

log (getInstancesCount ())