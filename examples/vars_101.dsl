var number a 15;
var number b a + 20;

print "a = ";
puti a;

print "b = ";
puti b;

var number c prompt "Pick a value for c";
var number d c + b;


print "c = ";
puti c;

print "c + b = d = ";
puti d;

var timestamp thefuture <2520-01-01> ;
var timestamp thepast   <1970-01-01> ;

if thefuture > thepast
	then (puts "the future is greater than the past!")
	else (puts "math is broken as we know it");

var timestamp today prompt "today's date/time?";

if thefuture < today
	then (puts "we are way past the future!")
	else (puts "we have not yet reached the future");

if thepast > today
	then (puts "we've gone waaaaay back in time")
	else (puts "the past is way behind us");