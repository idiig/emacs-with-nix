{ package_src
, elispFileVersion
, melpaBuild
, buildNpmPackage
, jq
}: melpaBuild (finalAttrs: let
  version = elispFileVersion "${finalAttrs.src}/eaf-browser.el";
  nodeName = "browser";
  nodeModules = buildNpmPackage rec {
    pname = nodeName;
    inherit version;
    src = package_src;
    npmDepsHash = "sha256-MUf+fJdEfzU/0e4he7mVURE1osP+Jm28LduCEtcJAPg=";
    prePatch = ''
      find . -mindepth 1 -maxdepth 1 ! -name "*.json" -exec rm -rf {} ';'
      ${jq}/bin/jq 'setpath(["name"]; "${pname}") | setpath(["version"]; "${version}")' package.json > package.json.tmp
      mv package.json.tmp package.json
    '';
    dontNpmBuild = true;
  };
in {
  pname = "eaf-browser";
  inherit version;
  src = package_src;
  postInstall = ''
    DST=$out/share/emacs/site-lisp/elpa/$ename-$melpaVersion/
    mv buffer.py $DST
    mv easylist.txt $DST
    ln -s ${nodeModules}/lib/node_modules/${nodeName}/node_modules $DST/node_modules
  '';
})
