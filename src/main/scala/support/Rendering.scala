package exp

import java.nio.file.Paths

import reftree.core.ToRefTree
import reftree.diagram.{Animation, Diagram}
import reftree.render.{Renderer, RenderingOptions}

// http://stanch.github.io/reftree/Guide.html#diagrams
// brew install graphviz

object Rendering {

  private val path = "/tmp/rendered"

  def animate[A: ToRefTree](animation: Animation.Builder[A], filename: String): Unit = {
    val renderer =
      Renderer(
        renderingOptions = RenderingOptions(density = 75),
        directory = Paths.get(path)
      )
    import renderer._

    animation
      .build()
      .render(filename)
  }

  def of[A: ToRefTree](a: A, filename: String): Unit = {
    val renderer =
      Renderer(
        renderingOptions = RenderingOptions(density = 75),
        directory = Paths.get(path)
      )

    import renderer._

    Diagram.sourceCodeCaption(a)
      .render(filename)
  }
}
