//! Background worker template

use super::{helpers, Template, TemplateCategory, TemplateOption, TemplateOptions};
use anyhow::Result;
use std::fs;
use std::path::Path;

pub struct WorkerTemplate;

impl Template for WorkerTemplate {
    fn name(&self) -> &'static str {
        "worker"
    }

    fn description(&self) -> &'static str {
        "Background job processor with queue integration"
    }

    fn aliases(&self) -> Vec<&'static str> {
        vec!["background", "job", "queue"]
    }

    fn category(&self) -> TemplateCategory {
        TemplateCategory::Service
    }

    fn options(&self) -> Vec<TemplateOption> {
        vec![
            TemplateOption {
                name: "queue",
                description: "Queue backend",
                default: Some("redis"),
                choices: vec!["redis", "rabbitmq", "kafka", "memory"],
            },
            TemplateOption {
                name: "scheduler",
                description: "Include job scheduler",
                default: Some("true"),
                choices: vec!["true", "false"],
            },
        ]
    }

    fn create(&self, path: &Path, name: &str, options: &TemplateOptions) -> Result<()> {
        let queue_type = options
            .custom
            .get("queue")
            .map(|s| s.as_str())
            .unwrap_or("redis");
        let with_scheduler = options
            .custom
            .get("scheduler")
            .map(|s| s.as_str())
            .unwrap_or("true")
            == "true";

        // Create project file
        let mut packages = vec![
            ("FluentAI.Worker", "1.0.0"),
            ("FluentAI.Queue", "1.0.0"),
            ("FluentAI.Logging", "1.0.0"),
            ("FluentAI.Config", "1.0.0"),
        ];

        if with_scheduler {
            packages.push(("FluentAI.Scheduler", "1.0.0"));
        }

        match queue_type {
            "redis" => packages.push(("FluentAI.Redis", "1.0.0")),
            "rabbitmq" => packages.push(("FluentAI.RabbitMQ", "1.0.0")),
            "kafka" => packages.push(("FluentAI.Kafka", "1.0.0")),
            _ => {}
        }

        helpers::create_project_file(path, name, "Exe", &packages)?;

        // Create main program
        let program_content = format!(
            r#";; {} Worker Service
;; Background job processor

(import "fluentai/worker" :as worker)
(import "fluentai/queue" :as queue)
(import "fluentai/logging" :as log)
(import "./src/config" :as config)
(import "./src/jobs" :as jobs)
(import "./src/middleware" :as middleware)
{}

;; Initialize application
(define init-app ()
  ;; Load configuration
  (config/load)
  
  ;; Initialize queue
  (queue/configure
    :backend :{}
    :connection (config/get :queue-url))
  
  ;; Set up logging
  (log/configure
    :level (config/get :log-level :info)
    :format :json)
  {}
  
  ;; Register job handlers
  (jobs/register-all))

;; Main entry point
(define main (args)
  (init-app)
  
  (let ([w (worker/create
             :name "{}-worker"
             :concurrency (config/get :worker-concurrency 5)
             :queues (config/get :worker-queues ["default" "priority"]))])
    
    ;; Apply middleware
    (worker/use w (middleware/error-handler))
    (worker/use w (middleware/logger))
    (worker/use w (middleware/metrics))
    
    ;; Start worker
    (log/info "Starting {} worker...")
    (worker/start w)
    
    ;; Handle shutdown gracefully
    (on-signal [:SIGTERM :SIGINT]
      (fn [sig]
        (log/info (format "Received {{}}, shutting down gracefully..." sig))
        (worker/stop w)
        (exit 0)))))

(when (= __name__ "__main__")
  (main (command-line-args)))
"#,
            name,
            if with_scheduler {
                "(import \"./src/scheduler\" :as scheduler)"
            } else {
                ""
            },
            queue_type,
            if with_scheduler {
                "\n  ;; Initialize scheduler\n  (scheduler/init)"
            } else {
                ""
            },
            name,
            name
        );
        fs::write(path.join("Program.ai"), program_content)?;

        // Create directories
        helpers::create_directories(
            path,
            &["src", "src/jobs", "src/processors", "tests", "scripts"],
        )?;

        // Create config module
        let config_content = r#";; Worker configuration

(module config
  (import "fluentai/env" :as env)
  
  (define config (atom {}))
  
  ;; Load configuration from environment
  (define load ()
    (reset! config
      {:queue-url (env/get "QUEUE_URL" "redis://localhost:6379")
       :worker-concurrency (env/get-int "WORKER_CONCURRENCY" 5)
       :worker-queues (env/get-list "WORKER_QUEUES" ["default"])
       :log-level (env/get "LOG_LEVEL" "info")
       :metrics-enabled (env/get-bool "METRICS_ENABLED" true)
       :retry-max-attempts (env/get-int "RETRY_MAX_ATTEMPTS" 3)
       :retry-backoff-base (env/get-int "RETRY_BACKOFF_BASE" 1000)}))
  
  ;; Get configuration value
  (define get 
    ([key] (get @config key))
    ([key default] (get @config key default)))
  
  (export load get))
"#;
        fs::write(path.join("src/config.ai"), config_content)?;

        // Create jobs registry
        let jobs_content = r#";; Job registry and registration

(module jobs
  (import "fluentai/worker" :as worker)
  (import "./jobs/email" :as email-jobs)
  (import "./jobs/data" :as data-jobs)
  (import "./jobs/notification" :as notification-jobs)
  
  ;; Register all job handlers
  (define register-all ()
    ;; Email jobs
    (worker/register "send-email" email-jobs/send-email)
    (worker/register "send-bulk-email" email-jobs/send-bulk-email)
    
    ;; Data processing jobs
    (worker/register "process-csv" data-jobs/process-csv)
    (worker/register "generate-report" data-jobs/generate-report)
    (worker/register "cleanup-old-data" data-jobs/cleanup-old-data)
    
    ;; Notification jobs
    (worker/register "send-push-notification" notification-jobs/send-push)
    (worker/register "send-sms" notification-jobs/send-sms))
  
  (export register-all))
"#;
        fs::write(path.join("src/jobs.ai"), jobs_content)?;

        // Create middleware
        let middleware_content = r#";; Worker middleware

(module middleware
  (import "fluentai/worker" :as worker)
  (import "fluentai/logging" :as log)
  (import "fluentai/metrics" :as metrics)
  
  ;; Error handling middleware
  (define error-handler ()
    (fn [job next]
      (try
        (next job)
        (catch e
          (log/error (format "Job {} failed: {}" 
                           (worker/job-id job)
                           (str e)))
          (metrics/increment "jobs.failed" 
                           {:job-type (worker/job-type job)})
          ;; Rethrow to trigger retry logic
          (throw e)))))
  
  ;; Logging middleware
  (define logger ()
    (fn [job next]
      (let ([start (current-time-ms)]
            [job-id (worker/job-id job)]
            [job-type (worker/job-type job)])
        (log/info (format "Processing job {}: {}" job-id job-type))
        (try
          (let ([result (next job)])
            (let ([duration (- (current-time-ms) start)])
              (log/info (format "Job {} completed in {}ms" job-id duration))
              (metrics/histogram "job.duration" duration
                               {:job-type job-type}))
            result)
          (catch e
            (log/error (format "Job {} failed after {}ms" 
                             job-id 
                             (- (current-time-ms) start)))
            (throw e))))))
  
  ;; Metrics collection middleware
  (define metrics ()
    (fn [job next]
      (metrics/increment "jobs.processed" 
                       {:job-type (worker/job-type job)})
      (metrics/gauge "jobs.active" 
                    (inc (metrics/get-gauge "jobs.active" 0)))
      (try
        (let ([result (next job)])
          (metrics/gauge "jobs.active" 
                        (dec (metrics/get-gauge "jobs.active" 0)))
          (metrics/increment "jobs.succeeded" 
                           {:job-type (worker/job-type job)})
          result)
        (catch e
          (metrics/gauge "jobs.active" 
                        (dec (metrics/get-gauge "jobs.active" 0)))
          (throw e)))))
  
  (export error-handler logger metrics))
"#;
        fs::write(path.join("src/middleware.ai"), middleware_content)?;

        // Create example email job
        let email_job = r#";; Email job handlers

(module email
  (import "fluentai/email" :as email)
  (import "fluentai/logging" :as log)
  (import "fluentai/validation" :as v)
  
  ;; Email job schema
  (define email-schema
    (v/object
      {:to (v/pipe (v/string) (v/email))
       :subject (v/string)
       :body (v/string)
       :from (v/optional (v/pipe (v/string) (v/email)))}))
  
  ;; Send single email
  (define send-email (job)
    (let ([data (worker/job-data job)]
          [errors (v/validate email-schema data)])
      (if (empty? errors)
          (do
            (email/send
              :to (:to data)
              :from (or (:from data) "noreply@example.com")
              :subject (:subject data)
              :body (:body data))
            (log/info (format "Email sent to {}" (:to data)))
            {:status :sent :to (:to data)})
          (do
            (log/error (format "Invalid email data: {}" errors))
            (throw (Error "Invalid email data"))))))
  
  ;; Send bulk emails
  (define send-bulk-email (job)
    (let ([data (worker/job-data job)]
          [recipients (:recipients data)]
          [template (:template data)])
      (log/info (format "Sending bulk email to {} recipients" 
                       (count recipients)))
      (let ([results (map 
                       (fn [recipient]
                         (try
                           (email/send-template
                             :to (:email recipient)
                             :template template
                             :data recipient)
                           {:email (:email recipient) :status :sent}
                           (catch e
                             {:email (:email recipient) 
                              :status :failed
                              :error (str e)})))
                       recipients)])
        (let ([sent (count (filter #(= (:status %) :sent) results))]
              [failed (count (filter #(= (:status %) :failed) results))])
          (log/info (format "Bulk email complete: {} sent, {} failed" 
                           sent failed))
          {:sent sent :failed failed :results results}))))
  
  (export send-email send-bulk-email))
"#;
        fs::write(path.join("src/jobs/email.ai"), email_job)?;

        // Create example data processing job
        let data_job = r#";; Data processing job handlers

(module data
  (import "fluentai/csv" :as csv)
  (import "fluentai/storage" :as storage)
  (import "fluentai/logging" :as log)
  (import "fluentai/db" :as db)
  
  ;; Process CSV file
  (define process-csv (job)
    (let ([data (worker/job-data job)]
          [file-url (:file-url data)]
          [process-fn (eval (:processor data))]) ; Name of processor function
      (log/info (format "Processing CSV file: {}" file-url))
      
      ;; Download file
      (let ([file-content (storage/download file-url)]
            [rows (csv/parse file-content)]
            [processed 0]
            [errors []]])
        
        ;; Process each row
        (doseq [row rows]
          (try
            (process-fn row)
            (set! processed (inc processed))
            (catch e
              (set! errors (conj errors {:row row :error (str e)})))))
        
        (log/info (format "CSV processing complete: {} rows processed, {} errors" 
                         processed (count errors)))
        {:processed processed :errors errors})))
  
  ;; Generate report
  (define generate-report (job)
    (let ([data (worker/job-data job)]
          [report-type (:type data)]
          [date-range (:date-range data)])
      (log/info (format "Generating {} report for {}" 
                       report-type date-range))
      
      ;; Query data
      (let ([report-data (case report-type
                           :sales (query-sales-data date-range)
                           :users (query-user-data date-range)
                           :performance (query-performance-data date-range)
                           (throw (Error (format "Unknown report type: {}" 
                                               report-type))))])
        
        ;; Generate report file
        (let ([report-file (generate-report-file report-type report-data)]
              [upload-url (storage/upload report-file 
                                        (format "reports/{}/{}.pdf" 
                                               report-type 
                                               (current-date)))])
          
          (log/info (format "Report generated: {}" upload-url))
          {:url upload-url :rows (count report-data)}))))
  
  ;; Cleanup old data
  (define cleanup-old-data (job)
    (let ([data (worker/job-data job)]
          [days-to-keep (or (:days data) 90)]
          [cutoff-date (subtract-days (current-date) days-to-keep)])
      (log/info (format "Cleaning up data older than {}" cutoff-date))
      
      (let ([deleted-logs (db/execute! 
                            "DELETE FROM logs WHERE created_at < ?" 
                            cutoff-date)]
            [deleted-sessions (db/execute!
                                "DELETE FROM sessions WHERE last_active < ?"
                                cutoff-date)]
            [deleted-temp (storage/cleanup-temp-files cutoff-date)])
        
        (log/info (format "Cleanup complete: {} logs, {} sessions, {} files" 
                         (:rows-affected deleted-logs)
                         (:rows-affected deleted-sessions)
                         deleted-temp))
        {:logs (:rows-affected deleted-logs)
         :sessions (:rows-affected deleted-sessions)
         :files deleted-temp})))
  
  ;; Helper functions
  (define query-sales-data (date-range)
    (db/query "SELECT * FROM sales WHERE created_at BETWEEN ? AND ?"
              (:start date-range) (:end date-range)))
  
  (define query-user-data (date-range)
    (db/query "SELECT * FROM users WHERE registered_at BETWEEN ? AND ?"
              (:start date-range) (:end date-range)))
  
  (define query-performance-data (date-range)
    (db/query "SELECT * FROM metrics WHERE timestamp BETWEEN ? AND ?"
              (:start date-range) (:end date-range)))
  
  (define generate-report-file (type data)
    ;; Implementation would generate actual report file
    (format "/tmp/report-{}-{}.pdf" type (current-timestamp)))
  
  (export process-csv generate-report cleanup-old-data))
"#;
        fs::write(path.join("src/jobs/data.ai"), data_job)?;

        // Create notification job
        let notification_job = r#";; Notification job handlers

(module notification
  (import "fluentai/push" :as push)
  (import "fluentai/sms" :as sms)
  (import "fluentai/logging" :as log)
  
  ;; Send push notification
  (define send-push (job)
    (let ([data (worker/job-data job)]
          [user-id (:user-id data)]
          [title (:title data)]
          [message (:message data)]
          [data-payload (:data data {})])
      (log/info (format "Sending push notification to user {}" user-id))
      
      ;; Get user's push tokens
      (let ([tokens (get-user-push-tokens user-id)])
        (if (empty? tokens)
            (do
              (log/warn (format "No push tokens found for user {}" user-id))
              {:status :no-tokens})
            (let ([results (map 
                             (fn [token]
                               (try
                                 (push/send
                                   :token token
                                   :title title
                                   :body message
                                   :data data-payload)
                                 {:token token :status :sent}
                                 (catch e
                                   {:token token 
                                    :status :failed
                                    :error (str e)})))
                             tokens)])
              (log/info (format "Push notification sent to {} tokens" 
                               (count (filter #(= (:status %) :sent) results))))
              {:results results})))))
  
  ;; Send SMS
  (define send-sms (job)
    (let ([data (worker/job-data job)]
          [phone (:phone data)]
          [message (:message data)])
      (log/info (format "Sending SMS to {}" phone))
      
      (try
        (let ([message-id (sms/send
                            :to phone
                            :body message
                            :from (config/get :sms-from-number))]])
          (log/info (format "SMS sent successfully: {}" message-id))
          {:status :sent :message-id message-id})
        (catch e
          (log/error (format "Failed to send SMS: {}" (str e)))
          (throw e)))))
  
  ;; Helper to get user's push tokens
  (define get-user-push-tokens (user-id)
    ;; In real implementation, would query database
    [])
  
  (export send-push send-sms))
"#;
        fs::write(path.join("src/jobs/notification.ai"), notification_job)?;

        // Create scheduler module if enabled
        if with_scheduler {
            let scheduler_content = r#";; Job scheduler

(module scheduler
  (import "fluentai/scheduler" :as sched)
  (import "fluentai/queue" :as queue)
  (import "fluentai/logging" :as log)
  
  ;; Initialize scheduler
  (define init ()
    (let ([s (sched/create)])
      
      ;; Daily cleanup job
      (sched/schedule s
        :name "daily-cleanup"
        :cron "0 2 * * *"  ; 2 AM daily
        :job (fn []
               (queue/enqueue "cleanup-old-data" 
                            {:days 90}
                            :queue "maintenance")))
      
      ;; Hourly metrics collection
      (sched/schedule s
        :name "collect-metrics"
        :cron "0 * * * *"  ; Every hour
        :job (fn []
               (queue/enqueue "collect-system-metrics" {})))
      
      ;; Weekly reports
      (sched/schedule s
        :name "weekly-reports"
        :cron "0 9 * * 1"  ; 9 AM every Monday
        :job (fn []
               (queue/enqueue "generate-report"
                            {:type :weekly
                             :date-range {:start (subtract-days (current-date) 7)
                                        :end (current-date)}}
                            :queue "reports")))
      
      ;; Start scheduler
      (log/info "Starting job scheduler...")
      (sched/start s)))
  
  (export init))
"#;
            fs::write(path.join("src/scheduler.ai"), scheduler_content)?;
        }

        // Create test
        let test_content = r#";; Worker tests

(import "fluentai/test" :as test)
(import "fluentai/worker/test" :as worker-test)
(import "../src/jobs/email" :as email-jobs)
(import "../src/jobs/data" :as data-jobs)

(test/describe "Worker Jobs"
  
  (test/describe "Email jobs"
    (test/it "sends single email"
      (let [job (worker-test/create-job
                  {:to "test@example.com"
                   :subject "Test Email"
                   :body "This is a test"})]
        (let [result (email-jobs/send-email job)]
          (test/expect (:status result) :to-be :sent)
          (test/expect (:to result) :to-be "test@example.com"))))
    
    (test/it "validates email data"
      (let [job (worker-test/create-job
                  {:to "invalid-email"
                   :subject "Test"
                   :body "Body"})]
        (test/expect (fn [] (email-jobs/send-email job))
                     :to-throw))))
  
  (test/describe "Data processing jobs"
    (test/it "processes CSV file"
      (worker-test/with-mock-storage
        (fn []
          (let [job (worker-test/create-job
                      {:file-url "s3://bucket/file.csv"
                       :processor "process-row"})]
            (let [result (data-jobs/process-csv job)]
              (test/expect (:processed result) :to-be-greater-than 0)
              (test/expect (:errors result) :to-be-empty))))))
    
    (test/it "generates reports"
      (let [job (worker-test/create-job
                  {:type :sales
                   :date-range {:start "2024-01-01"
                              :end "2024-01-31"}})]
        (let [result (data-jobs/generate-report job)]
          (test/expect (:url result) :to-contain "reports/sales/")
          (test/expect (:rows result) :to-be-a :number))))))
"#;
        fs::write(path.join("tests/worker.test.ai"), test_content)?;

        // Create docker-compose for queue backend
        if queue_type != "memory" {
            let docker_content = match queue_type {
                "redis" => {
                    r#"version: '3.8'

services:
  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis_data:/data
    command: redis-server --appendonly yes

volumes:
  redis_data:
"#
                }
                "rabbitmq" => {
                    r#"version: '3.8'

services:
  rabbitmq:
    image: rabbitmq:3-management
    ports:
      - "5672:5672"
      - "15672:15672"
    environment:
      RABBITMQ_DEFAULT_USER: admin
      RABBITMQ_DEFAULT_PASS: admin
    volumes:
      - rabbitmq_data:/var/lib/rabbitmq

volumes:
  rabbitmq_data:
"#
                }
                "kafka" => {
                    r#"version: '3.8'

services:
  zookeeper:
    image: confluentinc/cp-zookeeper:latest
    environment:
      ZOOKEEPER_CLIENT_PORT: 2181
      ZOOKEEPER_TICK_TIME: 2000

  kafka:
    image: confluentinc/cp-kafka:latest
    depends_on:
      - zookeeper
    ports:
      - "9092:9092"
    environment:
      KAFKA_BROKER_ID: 1
      KAFKA_ZOOKEEPER_CONNECT: zookeeper:2181
      KAFKA_ADVERTISED_LISTENERS: PLAINTEXT://localhost:9092
      KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR: 1
"#
                }
                _ => "",
            };

            if !docker_content.is_empty() {
                fs::write(path.join("docker-compose.yml"), docker_content)?;
            }
        }

        // Create .env.example
        let env_example = format!(
            r#"# Worker configuration

# Queue configuration
QUEUE_URL={}

# Worker settings
WORKER_CONCURRENCY=5
WORKER_QUEUES=default,priority,maintenance,reports

# Logging
LOG_LEVEL=info

# Metrics
METRICS_ENABLED=true

# Retry configuration
RETRY_MAX_ATTEMPTS=3
RETRY_BACKOFF_BASE=1000

# SMS configuration (if using SMS notifications)
SMS_FROM_NUMBER=+1234567890
"#,
            match queue_type {
                "redis" => "redis://localhost:6379",
                "rabbitmq" => "amqp://admin:admin@localhost:5672",
                "kafka" => "kafka://localhost:9092",
                _ => "memory://",
            }
        );
        fs::write(path.join(".env.example"), env_example)?;

        // Create deployment script
        let deploy_script = format!(
            r#"#!/bin/bash
# Deployment script for {} worker

set -e

echo "Deploying {} worker..."

# Build the worker
fluentai build -c Release

# Run database migrations if needed
# fluentai db migrate

# Deploy using your preferred method:
# - Docker: docker build -t {}-worker . && docker push ...
# - Systemd: sudo systemctl restart {}-worker
# - Kubernetes: kubectl apply -f k8s/

echo "Deployment complete!"
"#,
            name, name, name, name
        );
        fs::write(path.join("scripts/deploy.sh"), deploy_script)?;

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(path.join("scripts/deploy.sh"))?.permissions();
            perms.set_mode(0o755);
            fs::set_permissions(path.join("scripts/deploy.sh"), perms)?;
        }

        // Create README
        let readme_content = format!(
            r#"# {} Worker

A background job processing service built with FluentAI.

## Features

- [x] Multiple queue backend support ({})
- [x] Concurrent job processing
- [x] Job retry with exponential backoff
- [x] Structured logging
- [x] Metrics collection
- [x] Graceful shutdown
{}
- [x] Example job handlers (email, data processing, notifications)

## Getting Started

### Prerequisites

- FluentAI SDK 1.0+
- {} backend

### Installation

1. Copy `.env.example` to `.env` and configure
2. Start the queue backend:

```bash
docker-compose up -d
```

3. Install dependencies:

```bash
fluentai restore
```

### Running

```bash
fluentai run
```

The worker will start processing jobs from the configured queues.

### Adding New Jobs

1. Create a new job handler in `src/jobs/`:

```fluentai
(module my-job
  (import "fluentai/logging" :as log)
  
  (define process-my-job (job)
    (let ([data (worker/job-data job)])
      (log/info "Processing my job..." data)
      ;; Job logic here
      {{:status :completed}}))
  
  (export process-my-job))
```

2. Register it in `src/jobs.ai`:

```fluentai
(worker/register "my-job" my-job/process-my-job)
```

3. Enqueue jobs from your application:

```fluentai
(queue/enqueue "my-job" {{:param "value"}})
```

## Configuration

See `.env.example` for all configuration options.

### Queue Configuration

- **Redis**: Fast in-memory queue, good for most use cases
- **RabbitMQ**: Feature-rich message broker with routing and priorities
- **Kafka**: Distributed streaming platform for high-throughput scenarios
- **Memory**: In-memory queue for development (data lost on restart)

### Worker Configuration

- `WORKER_CONCURRENCY`: Number of concurrent jobs to process
- `WORKER_QUEUES`: Comma-separated list of queues to process
- `RETRY_MAX_ATTEMPTS`: Maximum retry attempts for failed jobs
- `RETRY_BACKOFF_BASE`: Base delay in ms for exponential backoff

## Monitoring

The worker exposes metrics that can be collected by Prometheus:

- `jobs.processed`: Total jobs processed
- `jobs.succeeded`: Successful jobs
- `jobs.failed`: Failed jobs
- `jobs.active`: Currently processing jobs
- `job.duration`: Job processing duration histogram

## Deployment

### Docker

```bash
docker build -t {}-worker .
docker run -d --env-file .env {}-worker
```

### Systemd

Create `/etc/systemd/system/{}-worker.service`:

```ini
[Unit]
Description={} Worker
After=network.target

[Service]
Type=simple
User=worker
WorkingDirectory=/opt/{}-worker
ExecStart=/usr/local/bin/fluentai run
Restart=always
EnvironmentFile=/opt/{}-worker/.env

[Install]
WantedBy=multi-user.target
```

### Kubernetes

See `k8s/` directory for Kubernetes manifests.

## License

MIT
"#,
            name,
            queue_type,
            if with_scheduler {
                "- [x] Cron-based job scheduling"
            } else {
                ""
            },
            match queue_type {
                "redis" => "Redis",
                "rabbitmq" => "RabbitMQ",
                "kafka" => "Apache Kafka",
                _ => "None",
            },
            name,
            name,
            name,
            name,
            name,
            name
        );

        fs::write(path.join("README.md"), readme_content)?;

        // Create .gitignore
        helpers::create_gitignore(path)?;

        Ok(())
    }
}
