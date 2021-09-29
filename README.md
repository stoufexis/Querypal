# Querypal
Querypal is a type-safe querying dsl built on top of **Doobie** and **Cats-effect** using **Scala 3**. It provides several conveniences and restrictions to the developer with the goal of helping you write complex queries while never allowing you to write an invalid one. Those include:

- **Compile time type checking**: Query pal makes  sure you can never write a statement that will fail due to a type mismatch (eg attempting to insert an int on a varchar column, or comparing between a string and an int in a condition)
- **Autocompletes**: At every step of the pipeline, querypal gives you helpful autocompletes. This combined with the strongly typed nature of the entire pipeline means that you are always given options of correct next steps while being prevented from writting incorrect ones. 
- **Auto-joins**: Quick join between two tables based on their foreign/primary keys 
- **Type level operations**: Querypal uses type level operations to achieve some of its features.
- **Object mapping**: Querypal can map a case class to a correct insert query

This project is a personal hobby project meant for practice and experimentation and thus is fairly barebones and lacks features that would make it a complete tool for DB interaction (eg The only types currently supported currrently are ``Int`` and ``String``, some sql operations arent supported etc). This can of course change in the future :-).  

*demo of usage:*
![complex-query-demo](https://user-images.githubusercontent.com/61254766/134810182-26433e92-276a-4b1e-ae11-20f4fbef7d5d.gif)


*the above code is compiled to the doobie fragment:*
```scala 
sql"select * from person where person.age > 13 or ( person.nickname like `The%` and person.age < 40 )"
```

## Notes
- **Similarities with slick**: During writting this project i was unaware of exactly how Slick queries are composed, but it turns out Querypal's and Slick's approach is very similar.


## Quick Start
*Note, familiarity with Scala 3, Doobie and Cats-effect is assumed throughout this doc*

Lets define a querypal model for one of our database tables

Given the table ```person```:
```sql
create table person  
(  
    name     varchar not null  constraint person_pk  primary key,  
    age      integer,  
    nickname varchar not null  
);
```
and our entity:
```scala
case  class  Person(name: String, age: Int, nickname: String)
```
*Note, the first member of the entity is assumed to be the primary key*

We can model it as:

```scala
//Model takes the table name as a parameter
//column is a protected helped method of the Model[_] trait. It receives a type parameter expressing the intended 
//type of the column and a its name on the database. Its used to correctly type the model's fields so they can be used type safely in other operations
object Person extends Model[Person]("person"):
  val name     = column[String]("name")
  val age      = column[Int]("age")
  val nickname = column[String]("nickname")
```
*We are placing this info on the companion object of the Person case class with the same name, this enables some very handy syntax when using the dsl*

Then we can derive a given instance of the ModelMeta type class for our Person entity.
```scala
//ModelMeta is a type class that holds meta-information about our Model and useful operations like object mapping. 
object Person extends Model[Person]("person"):
  val name     = column[String]("name")
  val age      = column[Int]("age")
  val nickname = column[String]("nickname")
  
  object Meta:
    given ModelMeta[Person] = deriveMeta(name, age, nickname)
```


### So what do we get from all that?

**The dsl**:
To start writting a query all we need to do is instanciate a ```QueryBuilder``` using our ```Person``` 
```scala
 QueryBuilder(Person)
```
Then we can construct the rest of our query using sql-like syntax:
```scala
QueryBuilder(Person) select * construct
```
Each query must end with ```construct```

We take advantage of scala's syntax sugars to achieve a syntax that looks very much like an sql query. If we get rid of all the syntax sugar that snippet is transformed to:
```scala
QueryBuilder(Person).select(*).construct
```
**So what is actually happening?**

Each method call defines a transformation of the query and returns the next step of the pipeline. Every step provides specific methods that allow transformations to that query. At the end we call construct to compile the query in the form of a doobie fragment that can be used to query the database, just like the fragments created with pure doobie. 
```scala 
val selectAll = QueryBuilder(Person) select * construct

for
	people <- selectAll.query[Person].to[List].transact(xa)
	_ <- IO.println(people)
yield  ExitCode.Success

//This is equal to
val selectAll = sql"select * from person"

for
	people <- selectAll.query[Person].to[List].transact(xa)
	_ <- IO.println(people)
yield  ExitCode.Success
```
### Object mapping
With query pal you get direct mapping of case classes to complete insert commands. 

```scala
QueryBuilder(Person) insert Person("John", 34, "The kid") construct

//it compiles to
sql"insert into person(name, age ,nickname)  values ('John', 34, 'The kid')"
```

### Autocompletes:
One of the biggest conveniences querypal that query pal provides is autocompletes. On every step you can get suggestions for your next move.

*simple autocomplete example:*

![demo_simple_autocomplete](https://user-images.githubusercontent.com/61254766/134810206-59813b22-f3af-4195-be3e-694961c14cf4.gif)


Where autocompletes shine the most is during composition of conditionals and complex queries.

![demo_autocompletes](https://user-images.githubusercontent.com/61254766/134810211-208b59df-32d3-4794-a621-fc9362f5fe2b.gif)


When you need to reference your table inside your query, querypal provides you with a lambda, giving you your model and expecting back a condition. Then using the underscore syntax for lambdas you can reference any of your fields (while getting useful autocompletes) and construct the condition using querypals operators resulting in a functional pipeline that closely resembles an sql query. 

Querypal also makes sure you cant write an invalid query, by only giving you access to the correct possible next steps

![demo_query_pipeline_checking](https://user-images.githubusercontent.com/61254766/134810220-8932b7cd-21c7-49c3-b927-7e4d68ae7107.gif)


### Type checking

When composing a complex query, its often the case that you tried to insert, compare or set a column with a value of a different type. Querypal's typed model fields and smart operators will let you know immediately when youve made such a mistake.

![demo_compile_errors](https://user-images.githubusercontent.com/61254766/134810233-30bacf3d-074a-4b27-995b-5ddac6c42810.gif)


 
### Relations and Joins

Given another table named ```photos```
```sql
create table photo  
(  
    	name              varchar not null  constraint photo_pk  primary key,  
	photographer_name varchar not null  constraint photographer__fk  references person  
);
```
We can see that theres a one to many relationship between ```person``` and ```photo```

Lets create our querypal model and derive our metadata
```scala
object Photo extends Model[Photo]("photo"):
  val name         = column[String]("name")
  val photographer = column[String]("photographer")

  object Meta:
    given ModelMeta[Photo] = deriveMeta(name, photographer)
```
Now lets define a relationship between ```person``` and ```photo```
```scala
//the parameter is the column to be used as the foreign key, referencing the primary key of our Person table
given  Relation[Photo, Person](Photo.photographer)
```
As a result, we can now compose join queries and have querypal check their validity
```scala
QueryBuilder(Person) select * join Photo construct
```
Querypals relations are expressed on the type level. Thus, it lets you know when attempting to join two tables that dont satisfy the constraint of a relation

![demo_join_typelevel](https://user-images.githubusercontent.com/61254766/134810907-e5452b08-e8cc-475b-b9a4-80439b87a685.gif)

### Unlimited Joins

Every Join operation initiates a select operation for the joined model. This means you can chain an unlimited amount of joins and select from the joined entities

To demonstrate, lets create a third enity/model
```scala
case class Pet(name: String, owner: String)

object Pet extends Model[Pet]("pet"):
  val name  = column[String]("name")
  val owner = column[Int]("owner_name")

  object Meta:
    given ModelMeta[Pet] = deriveMeta(name, owner)
    
given Relation[Pet, Person](Pet.owner)
```

Now we can join Photo and Pet to Person while selecting from them
```scala
//after joining with photo, you are selecting from photo.
//after joining with pet, you are selecting from pet
val multiJoin = QueryBuilder(Person) select (_.age > 20) join
      Photo select (_.name like "A Day in the%") join Pet select (_.name like "G%") construct
```
The above query compiles to
```scala
sql"select * from person inner join photo on photo.photographer_name = person.name inner join pet on pet.owner_name = person.name where person.age > 20 and photo.name like '%Bob' and pet.name like 'G%' "
```
*Note: Querypal uses inner joins exclusively*

### Automatic Table Creation
Having modeled our entities, we can easily derive table creation scripts for them and automatically set up our database stracture. 

Lets create a table for our Photo model.
```scala
//we need an semigroup to compose our ConnectionIOs returned by tableGen and
//relationGen to run them as one program
given [F[_]: Apply, A: Semigroup]: Semigroup[F[A]] = Apply.semigroup[F, A]

for _ <- (tableGen[Photo] |+| relationGen[Photo, Person]).transact(xa)
yield ExitCode.Success
```
Firstly, `tableGen[A]` creates our table, its primary key constraint and an index on that primary key. Then, `relationGen[A]` adds the foreign key constraint of `photographer_name` referencing the primary key of `person`.

Given our setup, this snippet creates the following table:
```sql
create table photo
(
    name              varchar not null constraint photo_pk primary key,
    photographer_name varchar not null constraint photo_photographer_name_fkey 
		references person
);

create unique index photo_name_uindex on photo (name);
``` 

### Snippets 
Below are provided a few code snippets using out above set up to demonstrate querypals features.

*basic insert:*
```scala
val  insertPerson = QueryBuilder(Person) insert Person("Jack", 34, "The kid") construct

for
	res <- insertPerson.update.run.transact(xa)
	_ <- IO.println(res)
yield  ExitCode.Success
```
*composed insert:*
```scala
val  insertPerson1 = (person: Person) => (QueryBuilder(Person) insert person construct).update.run.transact(xa)

for
	res1 <- insertPerson1(Person("Dennis", 12, "The Menace"))
	res2 <- insertPerson1(Person("George", 56, "Angry neighbour"))
	res3 <- insertPerson1(Person("Karen", 48, "Angrier Neighbour"))
yield  ExitCode.Success
```
*basic select:*
```scala
val  selectAll = QueryBuilder(Person) select * construct

for
	people <- selectAll.query[Person].to[List].transact(xa)
	_ <- IO.println(people)
yield  ExitCode.Success
```
*basic update:*
```scala
val  set = QueryBuilder(Person) update (_.age set 13) update (_.nickname set "Young Again") where (_.age > 45) construct

for
	res <- set.update.run.transact(xa)
	_ <- IO.println(res)
yield  ExitCode.Success
```
*basic delete:*
```scala
val  del = QueryBuilder(Person) delete (_.name === "Karen") or (_.name === "George") construct

for
	aa <- del.update.run.transact(xa)
	_ <- IO.println(aa)
yield  ExitCode.Success
```
*basic join and select:*
```scala
val  join = QueryBuilder(Person) join Photo select (_.age > 20) construct

for
	aa <- join.query[(Person, Photo)].to[List].transact(xa)
	_ <- IO.println(aa)
yield  ExitCode.Success
```
*complex select:*
```scala
 val  query = QueryBuilder(Person) select (_.age > 13) or (_.nickname like "The%") bind (_ and (_.age < 40)) construct

for
	aa <- query.query[Person].to[List].transact(xa)
	_ <- IO.println(aa)
yield  ExitCode.Success
```
