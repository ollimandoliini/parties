terraform {
  backend "gcs" {
    bucket = "eventti-tfstate"
    prefix = "env/dev"
  }
}