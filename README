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