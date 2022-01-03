provider "google" {
  project = "eventti"
  region  = "europe-north1"
  scopes = [
    "https://www.googleapis.com/auth/cloud-platform",
    "https://www.googleapis.com/auth/userinfo.email",
  ]
}

module "app" {
  source    = "../../modules/app"
  project   = "eventti"
  region    = "europe-north1"
  image_tag = var.image_tag
}