mkdir -p disk
mkdir -p disk/static/fonts && cp fonts/JosefinSans-SemiBold.ttf disk/static/fonts
bower update && bower install
lessc less/style.less disk/static/css/style.css --source-map-map-inline --strict-imports 
