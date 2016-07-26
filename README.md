# Ares

Ares is an application server for the [Microgram](https://github.com/zalora/microgram). We can use it to control different applications (for example, start and stop them, or get logs information) via simple commands.

Ares uses two concepts:

1. Application. There're many different applications we can work with via Ares.
2. Service. Ares supports only two services: [Angel](https://github.com/zalora/angel), daemon for the process monitoring/management, and [Nginx](https://github.com/zalora/microgram/tree/master/pkgs/nginx).

## Running

Basic usage is:

```bash
$ ares CONFIG
```

where `CONFIG` is a path to configuration file. Example of configuration file:

```json
{
  "angelPath": "/nix/store/28v1rlns1cnd2i0p9zyg4wx5rpdvih2m-angel-0.5.1/bin/angel",
  "appsDataDirRoot": "/var/apps",
  "builtinApps": {},
  "dataDir": "/var/ares",
  "nginxConfigFile": "/nix/store/7l4kgz8m0jh6l517jsy5c5fq45ivlrjd-ares-nginx.conf",
  "nginxEnable": true,
  "nginxPath": "/nix/store/bik80155gfarkx1ggakl7ykl5n37wldf-nginx-1.9.4/bin/nginx",
  "nginxRestartOnFailureDelay": 3000000,
  "nixEnvPath": "/nix/store/fnky09kvm0rm2ps9msiy6cznvnfxflbq-nix-1.10/bin/nix-env",
  "port": 13,
  "profilesDir": "/nix/var/nix/profiles/ares-apps",
  "runDir": "/run/apps/ares",
  "wtfdbFile": "/var/ares/wtfdb.json"
}
```

## API

Ares listens UNIX-socket, not TCP. You can use `curl` to make requests, for example via such a script:

```bash
curl -fsS --unix-socket "$RUN_DIR"/warp.sock -X "$METHOD" http:"$URL" "$ADDITIONAL_ARGS" | jq .
```

where:

- `$RUN_DIR` - path to directory where `ares` started from.
- `$METHOD` - request method, like `GET`, `PUT` or `DELETE`.
- `$URL` - request URL, like `/stop` or `/logs`.
- `$ADDITIONAL_ARGS` - additional arguments for `curl`.

### Start

Starts `ares` as a daemon.

Command:

```bash
$ nohup ares CONFIG 0<&- 1>ares.stdout.log 2>ares.stderr.log &
```

Output: none.

### Stop

Stops `ares`.

METHOD: `POST`
URL: `/stop`

Output: none.

### Install application

Adds application in `ares`.

METHOD: `PUT`
URL: `/apps/APP_NAME`
ADDITIONAL_ARGS: `-d APP_NIX_PATH`

where `APP_NIX_PATH` - Nix path to an application.

Output is an info about application, for example:

```json
{
  "appPath": "/nix/store/5afzijygf7r4dv1c88sdkrcs15x67kj7-memcached-default-runc-app",
  "needNginx": false,
  "needAngel": true,
  "appDataDir": "/var/apps/memcached-default",
  "diagsFile": "/nix/store/5afzijygf7r4dv1c88sdkrcs15x67kj7-memcached-default-runc-app/diags.json",
  "appName": "memcached-default",
  "profileDir": "/nix/var/nix/profiles/ares-apps/memcached-default",
  "cliFiles": {
    "memcached-default-ctl": "/nix/store/irf3i2f3ihf31dj0z50f6d2dhylgp143-memcached-default-ctl",
    "memcached-default-sh": "/nix/store/0kzryddchyjqp419xnqzi5zbys9wnyjz-memcached-default-sh"
  },
  "logFiles": [
    "/var/apps/memcached-default/log/main-process-stderr.log",
    "/var/apps/memcached-default/log/logrotate-stdout.log",
    "/var/apps/memcached-default/log/angel.log",
    "/var/apps/memcached-default/log/plugin.log",
    "/var/apps/memcached-default/log/logrotate-stderr.log",
    "/var/apps/memcached-default/log/main-process-stdout.log"
  ]
}
```

### Uninstall application

Remove application from `ares`.

METHOD: `DELETE`
URL: `/apps/APP_NAME`

where `APP_NAME` is the name of an application.

If application was uninstalled successfully, output is:

```bash
true
```

If not (for example, if there's no application with the name `APP_NAME`), output is:

```bash
false
```

### Factory reset

Reinstalls builtin applications.

METHOD: `POST`
URL: `/factory-reset`

Output: none.

### List of logs

Returns list of the paths to logs (produced by services and applications).

METHOD: `GET`
URL: `/logs`

Example output (partial):

```json
[
  "/var/apps/webservices/log/action.log",
  "/var/apps/webservices/log/angel.log",
  "/var/apps/webservices/log/bap_error.log",
  "/var/apps/webservices/log/housekeeping-process-stdout.log",
  "/var/apps/ares/log/ares.stderr.log",
  "/var/apps/ares/log/ares.stdout.log",
  "/var/apps/ares/log/default-nginx-access.json",
  "/var/apps/ares/log/default-nginx-error.log",
  ...
]
```

### List of applications

Returns info about applications currently served by `ares`.

METHOD: `GET`
URL: `/apps`

Example output (partial):

```json
[
  {
    "appPath": "/nix/store/nii08vpihw02d2baqn75ibrwr0hrfjyl-static-runc-app",
    "needNginx": true,
    "needAngel": true,
    "appDataDir": "/var/apps/static",
    "diagsFile": "/nix/store/nii08vpihw02d2baqn75ibrwr0hrfjyl-static-runc-app/diags.json",
    "appName": "static",
    "profileDir": "/nix/var/nix/profiles/ares-apps/static",
    "cliFiles": {
      "static-sh": "/nix/store/bl72v9f4256sgimn43ar93bgijgjhq7r-static-sh",
      "static-ctl": "/nix/store/3chh5k9irypkmsngr79ci4zc07bz10jr-static-ctl"
    },
    "logFiles": [
      "/var/apps/static/log/angel.log",
      "/var/apps/static/log/nginx-access.json",
      "/var/apps/static/log/nginx-error.log"
    ]
  },
  ...
]
```

Result list can be empty.

### Reload services

Reloads services.

METHOD: `POST`
URL: `/reload`

Output: none.

### Ares WTF

Runs `wtf`-command based on `wtfdb.json` in `ares`-data directory. For more info about `wtf` please see [its repository](https://github.com/zalora/microgram/tree/master/wtf).

Command:

```bash
WTFDB=PATH_TO_WTFDB_JSON exec wtf ARGS
```

where `PATH_TO_WTFDB_JSON` is a path to `wtfdb.json` and `ARGS` is a list of optional `wtf`-arguments.

Output: `wtf`-output, for example

```bash
static-catalog-keeps-spaces: --> OK
static-catalog-accessible: --> OK
sessions-sessions-evictions: --> 0
sessions-sessions-total_items: --> 0
healthcheck: --> OK
worker-available-timestamp: --> OK
```

