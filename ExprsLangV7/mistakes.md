+ Maybe add non-ptr word type to triv so we can represent non ptr values properly.
  This is motivated by incrementing and decrementing the frame pointer.
+ True and false do actually have different tags and so should be treated as such.
