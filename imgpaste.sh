#!/bin/sh
curl -w %{redirect_url} -F "var=@$1;type=image/png" -F "tag=$2" "http://localhost:3000/api/upload"
