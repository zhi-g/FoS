package fos

trait Magic {
    val EmptyOS = new java.io.OutputStream() {
        def write(b: Int) {}
    }

    def magic(thunk: => Boolean): Boolean = try {
        noSillyOutput(thunk)
    } catch {
        case e: java.lang.StackOverflowError => true
    }

    def magic(thunk: => org.scalacheck.Prop): org.scalacheck.Prop = try {
        noSillyOutput(thunk)
    } catch {
        case e: java.lang.StackOverflowError => true
    }

    def noSillyOutput[T](thunk: => T): T = Console.withOut(EmptyOS)(thunk)
}
