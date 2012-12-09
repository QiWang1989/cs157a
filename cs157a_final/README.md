This project is a general inventory management system.

I. Features

In core functionality, it supports the following operations:

1. add a record to the inventory. Number of copy defaults to 1.
val lf = new LibraryFrontend
lf.add(LibraryEntry("no1","life of pie", "Yann Martel"), copy)

2. add more copies to an existing record.
val lf = new LibraryFrontend
lf.addCopy("no1", 10)

3. delete a record.
val lf = new LibraryFrontend
lf.delete("no1")

4. delete some number of copies of a record.
val lf = new LibraryFrontend
lf.deleteCopy("no1", 3)

In addition to operations above, a more specific inventory system may support other operations.
For example, library system should support borrow and return a book.
1. borrow a book
val lf = new LibraryFrontend
lf.borrowBook("no1", "QiQi")

2. return a book
val lf = new LibraryFrontend
lf.returnBook("no1", "QiQi")

II. Class hierarchy
1. Trait InventoryDB and Frontend defines general core functionalities.
2. Both LibraryInventoryDB and ApparelInventoryDB extends from InventoryDB. 
3. Both LibraryFrontend and ApparelFrontend extends from Frontend.
4. Frontend requires (scala self type) InventoryDB. Therefore, LibraryFrontend mix in LibraryInventoryDB, and ApparelFrontend mix in ApparelInventoryDB.