

resource "google_cloud_run_service" "event_app" {
  name     = "event-app"
  location = "europe-north1"

  template {
    spec {
      containers {
        image = "gcr.io/${var.project}/event-app:${var.image_tag}"
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }
}
