/*
 *  @author Philip Stutz
 *
 *  Copyright 2011 University of Zurich
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package com.signalcollect.examples

import com.signalcollect._

/**
 *  Regular expression to match links in Html strings
 */
object RegexEx {
  val hyperlink = """<a\s+href=(?:"([^"]+)"|'([^']+)').*?</a>""".r
}

/**
 *  Proof of concept combination of the PageRank vertices with a WebCrawler
 *  Do not use on a larger scale:
 *  	- does not respect robots.txt
 *  	- lacks proper user agent string
 */
object WebCrawlerEx extends App {
  val graph = new GraphBuilder[String, Double].build
  graph.addVertex(new WebpageEx("http://www.ifi.uzh.ch/ddis/", 2))
  val stats = graph.execute
  graph.foreachVertex(println(_))
  println(stats)
  graph.shutdown
}

/**
 *  Adds linked webpages as vertices to the graph and connects them with a link edge
 */
class WebpageEx(id: String, crawlDepth: Int, dampingFactor: Double = 0.85) extends PageRankVertexEx(id, dampingFactor) {

  /** This method gets called by the framework after the vertex has been fully initialized. */
  override def afterInitialization(graphEditor: GraphEditor[String, Double]) {
    super.afterInitialization(graphEditor)
    if (crawlDepth > 0) {
      try {
        val webpage = io.Source.fromURL(id, "ISO-8859-1").mkString
        RegexEx.hyperlink.findAllIn(webpage).matchData map (_.group(1)) foreach { linked =>
          graphEditor.addVertex(new WebpageEx(linked, crawlDepth - 1))
          graphEditor.addEdge(id, new PageRankEdgeEx(linked))
        }
      } catch {
        case _: Throwable =>
      }
    }
  }
}