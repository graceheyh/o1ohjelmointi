package o1.messages

// This code is explained in Chapter 7.4.

trait Message(val content: String, val isPublic: Boolean):
  override def toString = this.content


class DirectMessage(content: String, val recipient: String) extends Message(content, false):
  override def toString = s"@$recipient: $content"

class Post(content: String, public: Boolean) extends Message(content, public)

class Comment(content: String, original: Post, public: Boolean) extends Message(content, public), Reply(original)

trait Reply(val toWhat: Message) extends Message:
  override def toString = s"$content (Re: ${toWhat.content})"


@main def testMessages() =
  def report(m: Message) = s"$m / ${if m.isPublic then "public" else "not public"}"
  val testDM = DirectMessage("Hello, it's me", "Adele")
  println(report(testDM))    // should print: @Adele: Hello, it's me / not public
  val testPost = Post("Ave, Munde!", true)
  println(report(testPost))  // should print: Ave, Munde! / public
  val testReply = Comment("Ave at thee, too!", testPost, false)


