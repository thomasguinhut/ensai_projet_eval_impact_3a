catnat <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "projet_eval_impact/catnat_gaspar.csv",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(catnat)
