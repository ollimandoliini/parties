
resource "google_compute_network" "private_network" {
  name                    = "events-network"
  auto_create_subnetworks = false
}

resource "google_compute_subnetwork" "subnetwork" {
  name          = "events-subnetwork"
  ip_cidr_range = "10.1.0.0/16"
  network       = google_compute_network.private_network.id
}

resource "google_vpc_access_connector" "connector" {
  name          = "connector"
  region        = var.region
  ip_cidr_range = "10.2.0.0/28"
  network       = google_compute_network.private_network.name
  depends_on    = [google_project_service.vpc_access]
}

resource "google_project_service" "vpc_access" {
  service            = "vpcaccess.googleapis.com"
  disable_on_destroy = false
}

resource "google_compute_global_address" "private_ip_address" {
  name          = "db-ip-address"
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = google_compute_network.private_network.id
}

resource "google_service_networking_connection" "private_vpc_connection" {
  network                 = google_compute_network.private_network.id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_ip_address.name]
}

resource "google_compute_firewall" "outbound_https_access" {
  name    = "outbound-https"
  network = google_compute_network.private_network.name

  allow {
    protocol = "tcp"
    ports    = ["443"]
  }

  direction          = "EGRESS"
  destination_ranges = ["34.120.43.199/32", "34.102.130.52/32"]
}

resource "google_compute_router" "router" {
  name    = "router"
  region  = var.region
  network = google_compute_network.private_network.id

  bgp {
    asn = 64514
  }
}

resource "google_compute_router_nat" "nat" {
  name                               = "router-nat"
  router                             = google_compute_router.router.name
  region                             = google_compute_router.router.region
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"

  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }
}



resource "google_service_account" "app" {
  account_id   = "event-app"
  display_name = "Event App Service Account"
}

resource "google_project_iam_member" "cloudsql_client_binding" {
  project = var.project
  role    = "roles/cloudsql.client"
  member  = "serviceAccount:${google_service_account.app.email}"
}

resource "google_cloud_run_service" "event_app" {
  name     = "event-app"
  location = "europe-north1"
  template {
    spec {
      service_account_name = google_service_account.app.email
      containers {
        image = "eu.gcr.io/${var.project}/event-app:${var.image_tag}"
        env {
          name  = "DB_CONNECTION_STRING"
          value = "\"postgresql://${google_sql_user.db_user.name}:${google_sql_user.db_user.password}@${google_sql_database_instance.instance.private_ip_address}:5432/${google_sql_database.database.name}\""
        }
      }
    }
    metadata {
      annotations = {
        "run.googleapis.com/vpc-access-connector" = google_vpc_access_connector.connector.id
        "run.googleapis.com/vpc-access-egress"    = "all"
      }
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
  region           = var.region
  database_version = "POSTGRES_13"
  settings {
    tier = "db-f1-micro"
    ip_configuration {
      ipv4_enabled    = false
      private_network = google_compute_network.private_network.id
    }
  }
  deletion_protection = "true"
  depends_on          = [google_service_networking_connection.private_vpc_connection]
}

resource "random_password" "db_user_password" {
  length  = 16
  special = false
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
  location = google_cloud_run_service.event_app.location
  project  = google_cloud_run_service.event_app.project
  service  = google_cloud_run_service.event_app.name

  policy_data = data.google_iam_policy.noauth.policy_data
}
