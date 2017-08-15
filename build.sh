# build script 
purs --version

echo "purs compile"
purs compile --dump-corefn bower_components/purescript-*/src/**/*.purs src/**/*.purs
echo "purs dce -e Jam.App.main"
purs dce -e Jam.App.main -o app-output
echo "purs dce -e Jam.Server.main"
purs dce -e Jam.Server.main -o server-output

echo "browserify & uglifyify app"
browserify -p [css-modulesify -o dist/static/style.css --after postcss-import --after postcss-url --after postcss-assets --after postcss-cssnext --after postcss-nested --after postcss-custom-media --after postcss-will-change --after postcss-custom-media ] app.js | uglifyjs -c > dist/static/index.js
echo "browserify & uglifyify server"
browserify --node -p [css-modulesify -o dist/static/style.css --after postcss-import --after postcss-url --after postcss-assets --after postcss-cssnext --after postcss-nested --after postcss-custom-media --after postcss-will-change --after postcss-custom-media ] server.js | uglifyjs -c > run.js
