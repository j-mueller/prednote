# Where to put QuickCheck instances?

I have long struggled with the question of where to put QuickCheck
Arbitrary (and, for that matter, CoArbitrary) instances.

## QuickCheck relies on typeclasses

Idiomatic use of QuickCheck relies on use of the Arbitrary and
CoArbitrary typeclasses.  As we'll see below, you can use QuickCheck
without using those typeclasses, but it will make your tests more
verbose and, therefore, more difficult to write.

With that in mind, most libraries of any size contain their own data
types.  So the question is: how do you declare Arbitrary and
CoArbitrary instances for those types?  A few choices come to mind:

## Create separate orphan instances

Since I don't control the QuickCheck library, I can't add my Arbitrary
and CoArbitrary instances to the the place where the typeclasses are
defined.  I can, however, create my own orphan instances.  Orphan
instances are generally considered bad; however, some say they are OK
if you are putting them in an executable, not a library.

The orphan approach might be OK if you do keep all your tests in an
executable.  However, what if someone wants to re-use the Arbitrary
and CoArbitrary instances from one package?  Then you need to have a
separate package just with orphan instances, which is a pain to
maintain.  Alternatively, the downstream libraries can just
re-implement the Arbitrary and CoArbitrary instances, which is also a
pain to maintain.

The advantage to this approach is that the main library does not incur
a dependency on QuickCheck.  The fewer dependencies the main library
has, the better.

## Create instances in the same modules as the original types

Orphans are bad.  So why create orphans?  Why not just put the
instances in the modules in which the types were defined?

The advantage here is: no orphans.  It's also the easiest to maintain,
as the Arbitrary and CoArbitrary instances are right alongside the
types and are easier to maintain that way.

There are a couple of disadvantages.  First, the main library now has
a dependency on QuickCheck.  This is more likely to cause problems for
downstream users.  Many downstream users don't even care about
testing, so a cost is imposed on them for something that doesn't
directly benefit them.  Second, the Arbitrary instances are now inside
of the modules that contain the types, even for types whose
contructors are not exported.  This means that the Arbitrary instance
might violate some of the type's invariants.  The only way to be sure
this doesn't happen is to test the Arbitrary instance's functions to
make sure they respect invariants.  When the Arbitrary instance is in
a separate module, we don't (usually) have to worry about that
instance violating invariants, because then Arbitrary doesn't have any
access to non-exported constructors.

## Create instances in the same modules as the original types, but use
   CPP to remove them from production code

This is the same as above, but we use CPP to strip the Arbitrary
instances out of production code and to add Arbitrary instances to
testing code.

The advantage is that production code need not incur a QuickCheck
dependency, so users who don't care about testing don't pay a penalty
for it.  One disadvantage is that now you have to deal with CPP.  In
addition, there's no way for dependent packages to specify that this
package must be built with a particular compiler flag in order to
enable the tests and, therefore, to export the Arbitrary instances.
Thus with this system it is hard to have dependent packages that
depend on the Arbitrary instances.

## Don't create Arbitrary and CoArbitrary instances at all

This is the idea behind the barecheck library.  Idiomatic QuickCheck
usage depends on the Arbitrary and CoArbitrary classes, but all the
options above this one (all of which rely on instances) create some
problems.  Why not just avoid typeclasses?

QuickCheck is perfectly usable in this way (in contrast, from what I
recall, Smallcheck will give you some grief if you try this.)  You can
use combinators such as "forAll" and "forAllShrink" to create
properties.  There were some gaps, which barecheck helps to fill.

The advantage here is that, without typeclass instances at all, you
don't have to worry about instances and orphans.  The disadvantage is
that writing properties using "forAll" and its brethren can get quite tedious.
A property that may have taken a couple of lines if written using
types that are instances of Arbitrary can now take several more lines
to write using a bunch of "forAll" combinators.  And if you want to
shrink?  Then you need "forAllShrink".  That starts getting nasty
really fast.  You also need a uniform naming scheme for the functions
and values you create, or the whole mess becomes too hard to handle.

## Use newtype wrappers

Sometimes the answer to orphan instances is to write newtypes;
QuickCheck is no exception.  You could take all the library's types
and wrap them in a newtype.  A naming convention such as ending each
newtype created for these purposes with a capital "A" can help.

This does allow idiomatic QuickCheck usage, sort of.  But now you have
to do all that wrapping or unwrapping, which can get tedious.  For
simple tests it's not that hard, but it starts getting nasty when you
have one type that is a product of several different types, some of
which also use the newtype method.  All this nested wrapping and
unwrapping grows unwieldy surprisingly quickly.

