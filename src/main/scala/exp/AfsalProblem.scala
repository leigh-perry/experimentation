package exp

object AfsalProblem {

  // A writeOperation(key, value, modeOfWriting) … this can return a Result . The Result should encapsulate the
  // actual return value and the mode. The Mode can be Overwrite (which can be Replaced or New or Failed),
  // Insert (which can be New or Failed) or Replace (which can be Wrote or Failed). Feel free to represent this in
  // any way which is type safe.
  sealed trait WriteMode
  object WriteMode {
    final case object Overwrite extends WriteMode
    final case object Insert extends WriteMode
    final case object Replace extends WriteMode
  }

  sealed trait WriteOutcome
  object WriteOutcome {
    final case object Replaced extends WriteOutcome
    final case object New extends WriteOutcome
    final case object Failed extends WriteOutcome
    final case object Wrote extends WriteOutcome
  }

  // TODO: The Result also has FaiureFallBackOperations or IfSuccess encoded in as well. This may or may not be
  // used by the clients using DbOperation.
  final case class Result[V](value: V, wr: WriteOutcome)
  final case class Page[K, V](values: List[V], nextKey: K)

  trait DbOperation[K, V] {
    def get(key: K): Option[V]
    def readAll: Page[K, V]
    def write(key: K, value: V, mode: WriteMode): Result[V]
    def delete(key: K): Unit
  }

  sealed trait Handler
  object Handler {
    final case object PrintLn extends Handler
    final case class Concatenate(s: String) extends Handler
    final case object WriteResultsToDb extends Handler
  }

  sealed trait AnotherHandler
  object AnotherHandle {
    final case object WriteToFile extends AnotherHandler
    final case object WriteToInMemory extends AnotherHandler
    final case object Etc extends AnotherHandler
  }

  final case class ReadAllResult[K, V](page: Page[K, V], handle: AnotherHandler)

  trait TableResult[K, V]
  final case class GetResult[V](value: V, handle: Handler)

  trait Table[K, V] {
    // The user can then do (in a different space, time) to actually do the handle operation.
    def get(key: K): Option[GetResult[V]]

    // Depending on the Result of operation you need to send a notification to admin which can be
    // sending an email, sending a message or only logging it
    def write(key: K, value: V, mode: WriteMode): TableResult[K, V]

    // If Mode == Fail, send a notification. If Mode is Replace success, then create an entry in success
    // control table. Else create an entry in failure control table.
    def replace(key: K, value: V, mode: WriteMode): TableResult[K, V]

    def delete(key: K): Unit

    // readAll returns page and an AnotherHandler. Example, based on the contents of the page, or size of page,
    // or based on some internal db operation, return the callback operation Write to a File or Write to InMemory
    // mutable map. Feel free to think we have dozens of different Handlers or CallBacks, which is returned
    // by different functions.
    def readAll: Page[K, V]

    def updateAndDelete(): TableResult[K, V] = ???
    def updateAndThenRead(): TableResult[K, V] = ???
    def deleteThenPut(): TableResult[K, V] = ???
  }

}
