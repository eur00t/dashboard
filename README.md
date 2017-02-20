Live at https://xxlv.party

## Conversations processor

Groups consecutive messages into conversations

![conv img](https://github.com/eur00t/dashboard/raw/master/img/conv.png)

## Frequency chart processor

Displays timeseries chart, a value at each point specifies a number of messages in some period before point (defined by configuration).

![freq img](https://github.com/eur00t/dashboard/raw/master/img/freq.png)

## Setup

1. Add `config.json` to the repo root:

        {
            "access_token": "<VK API token with permanent messages access>",
            "api_version": "5.60",
            "api_url": "https://api.vk.com/method/"
        }

2. Modify `app_config.ml` (if you want TLS).

3. Fetch deps with Opam, build:

        opam switch 4.04.0
        opam pin add -y reactjs git@github.com:fxfactorial/ocaml-reactjs.git
        opam pin add xxlv ./

        make build
        make build_client

4. Serve front-end from `/public`, e.g. with Nginx:

        http {
            include /usr/local/etc/nginx/mime.types;

            server {
                listen 8080;
                root <local repo path>/public;

                location / {
                }

                location /cached {
                    expires 1y;
                }
            }
        }

        events {
        }

5. Run `./main.native <dump file path>`