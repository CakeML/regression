structure Config = struct
  val github_user   = "CakeML"
  val github_repo   = "cakeml"
  val cakeml_github = String.concat ["https://github.com/",
                                     github_user, "/",
                                     github_repo]
  val hol_github    = "https://github.com/HOL-Theorem-Prover/HOL"
  val host          = "https://cakeml.org"
  val base_url      = "/regression.cgi"
  val to_address    = "builds@cakeml.org"
end
