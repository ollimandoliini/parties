

resource "google_cloud_run_service" "event_app" {
  name     = "event-app"
  location = "europe-north1"
  template {
    spec {
      containers {
        image = "gcr.io/${var.project}/event-app:${var.image_tag}"
        env {
          # postgres://<pg_user>:<pg_pass>@/<db_name>?host=/cloudsql/<cloud_sql_instance_connection_name>
          DB_CONNECTION_STRING = "postgres://${google_sql_user.db_user.name}:${google_sql_user.db_user.password}@/${google_sql_database.database.name}?host=/cloudsql/${google_sql_database_instance.instance.connection_name}"
        }
      }
    }
  }
  metadata {
    annotations = {
      "run.googleapis.com/cloudsql-instances" = google_sql_database_instance.instance.connection_name
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }
}

resource "google_sql_database" "database" {
  name     = "events-app-database"
  instance = google_sql_database_instance.instance.name
}

resource "google_sql_database_instance" "instance" {
  name             = "events-app-database-instance"
  region           = "europe-north1"
  database_version = "POSTGRES_13"
  settings {
    tier = "db-f1-micro"
  }
  deletion_protection  = "true"
}

resource "random_password" "db_user_password" {
  length = 16
}

resource "google_sql_user" "db_user" {
  instance = google_sql_database_instance.instance.name
  name     = "user"
  password = random_password.db_user_password.result
}

data "google_iam_policy" "noauth" {
  binding {
    role = "roles/run.invoker"
    members = [
      "allUsers",
    ]
  }
}

resource "google_cloud_run_service_iam_policy" "noauth" {
  location    = google_cloud_run_service.event_app.location
  project     = google_cloud_run_service.event_app.project
  service     = google_cloud_run_service.event_app.name

  policy_data = data.google_iam_policy.noauth.policy_data
}
