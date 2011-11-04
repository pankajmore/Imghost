#!bin/sh
cd ~/nsfw 
for f in `dir -d *` ; 
    do
        echo "$f"
        curl -v -F "var=@$f;type=image/png" -F "tag=NSFW" "http://localhost:5432/api/upload"
done

