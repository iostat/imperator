// making these as variables as a demo, though they could just as easily work as constexprs
var number smallShipmentDiscountCutoff 1000;
var number smallShipmentDiscountValue 1000;
var timestamp latePaymentCutoff <2018-01-01>;
var number latePaymentPenalty 1000;
var number doubleOrNothingPenalty 1000;

// prompt inputs from end user
var number baseInvoiceValue prompt "Base Invoice Value";
var timestamp paymentDate prompt "Payment Date";
var number shippedItemsCount prompt "Number of shipped items";

var number invoice baseInvoiceValue;

// req 1
if shippedItemsCount < smallShipmentDiscountCutoff
	then (set invoice (invoice - smallShipmentDiscountValue))
	else (nop);

print "Invoice after req 1 is: ";
puti invoice;

// req 2
if paymentDate >= latePaymentCutoff
    then (set invoice (invoice + latePaymentPenalty))
    else (nop);

print "Invoice after req 2 is: ";
puti invoice;

// req 3: compose em!
if shippedItemsCount >= smallShipmentDiscountCutoff || paymentDate >= latePaymentCutoff
    then (set invoice (invoice + doubleOrNothingPenalty))
    else (set invoice (invoice - doubleOrNothingPenalty));

print "Invoice after req 3 is: ";
puti invoice