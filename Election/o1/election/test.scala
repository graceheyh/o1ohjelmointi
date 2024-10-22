package o1.election

/** A small program that uses the classes in this package. May be customized by the student. */
@main def testElection() =

  val candidates = Vector(
    Candidate("Outi Alanko-Kahiluoto", 2590, "VIHR"),
    Candidate("Pentti Arajärvi",       3682, "SDP"),
    Candidate("Paavo Arhinmäki",       6467, "VAS"),
    Candidate("Sirpa Asko-Seljavaara", 2250, "KOK"),
    Candidate("Juha Hakola",           2571, "KOK"),
    Candidate("Jussi Halla-aho",       6034, "PS"),
    Candidate("Heidi Hautala",         2916, "VIHR"),
    Candidate("Eero Heinäluoma",       5243, "SDP"),
    Candidate("Päivi Lipponen",        1675, "SDP"),
    Candidate("Silvia Modig",          2003, "VAS"),
    Candidate("Lasse Männistö",        2469, "KOK"),
    Candidate("Jaana Pelkonen",        3156, "KOK"),
    Candidate("Tom Packalén",          1928, "PS"),
    Candidate("Wille Rydman",          2241, "KOK"),
    Candidate("Anni Sinnemäki",        2609, "VIHR"),
    Candidate("Osmo Soininvaara",      5889, "VIHR"),
    Candidate("Johanna Sumuvuori",     2180, "VIHR"),
    Candidate("Astrid Thors",          2298, "RKP"),
    Candidate("Jan Vapaavuori",        7894, "KOK")
  )
  val helsinki = District("Mini-Helsinki", 8, candidates)
  println(helsinki)

  println("\nA list of all the candidates:")
  helsinki.printCandidates()

  println("\nCandidates from KOK and PS:")
  println(helsinki.candidatesFrom("KOK").mkString("\t"))
  println(helsinki.candidatesFrom("PS").mkString("\t"))

  println("\nTotal number of votes: " + helsinki.totalVotes)
  println("\nThe candidate with the most votes: " + helsinki.topCandidate)

  println("\nTotal votes for VAS: " + helsinki.totalVotes("VAS"))
  println("Total votes for RKP: " + helsinki.totalVotes("RKP"))

  // THE FOLLOWING CODE IS ASSOCIATED WITH CHAPTER 10.1.
  // DON’T UNCOMMENT IT BEFORE YOU REACH THAT CHAPTER.

  /*
  // 1/2

  println("\nCandidates for each party:")
  println(helsinki.candidatesByParty.values.map( _.mkString("\t") ).mkString("\n"))

  println("\nTop candidate for each party:")
  println(helsinki.topCandidatesByParty.values.mkString("\t"))

  println("\nTotal number of votes for each party:")
  println(helsinki.votesByParty.mkString("\t"))
  */

  /*
  // 2/2

  println("\nParties ordered by total votes:")
  println(helsinki.rankingOfParties.mkString(", "))

  println("\nThe candidates ranked within their respective parties:")
  val rankingMap = helsinki.rankingsWithinParties.map( entry => entry(0) -> entry(1).mkString("\t") )
  println(rankingMap.mkString("\n"))

  println("\nThe distribution figures for each candidate:")
  println(helsinki.distributionFigures.mkString("\n"))

  println("\nThe elected candidates, in order by distribution figure:")
  println(helsinki.electedCandidates.map( _.name ).mkString(", "))
   */

end testElection

