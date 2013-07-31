Add instructions that the TA can follow to grade your project. 

-> Compile encroach and run it!
THINGS IMPLEMENTED / CHANGES TO ENCROACH
----------------------------------------
. VISUAL CHANGES
- rad intro screen woo. adding some plot/ motivation.
- added boundaries 
  - purely ascetic and because the lack thereof annoyed me
  - space invaders don't cross boundaries 
- changed minimum screen size
- some random ascetic tweaks all around i can't remember
. FUNCTIONALITY CHANGES
- encroachers can "return fire" semi-randomly 
  - not really randomly, because trying to create pseudo-random behavior with, like, the mod function alone was doomed to failure. 
  - at least it looks plausible!
- the player has three lives. 
  - countdown displayed in upper left corner
  - once player is hit by return fire three times, the game is over
- game over screen with option to replay or exit


THINGS IMPLEMENTED / CHANGES TO OATLANG!
----------------------------------------
Syntactic sugar to eliminate common `if?` + `cast` idiom in OAT. Use `castnull`!

```c
if? (Object o = item.obj) {
  cast (Foo f = o) {
    f.print();
  } else print_string("failed cast");
} else print_string("failed null");
```
    
becomes
    
    castnull (Foo f = item.obj) {
      f.print();
    } else print_string("failed null/cast");

Or, in `encroach.oat`:

```c
if?( Object o = item.obj ) {
  cast (DelayedObject dobj = o) {
    dobj.draw();
  }
}
```

becomes

```c
castnull ( DelayedObject dobj = item.obj ) {
  dobj.draw();
}
```

This is essentially a modification to the parser that unrolls a `castnull` into an `if?` dereference followed by a `cast` from Object. This makese the pseudo-polymorphism used in a datatype like `List` work a bit better/cleaner.

To see that the syntax doesn't change the program semantics, compile `tests/encroach.oat` and `tests/encroach_orig.oat`, diff them, and see that the same LL code is generated for both.
