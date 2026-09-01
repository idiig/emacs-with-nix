{ package_src
, elispFileVersion
, lib
, melpaBuild
, fetchFromGitHub
, msgpack
, pkgsCross
}: let
  trampVersion = "2.8.2";

  trampForTrampRpc = melpaBuild {
    pname = "tramp";
    version = trampVersion;
    src = fetchFromGitHub {
      owner = "emacsmirror";
      repo = "tramp";
      rev = "904779d264156bfb4ecfc7fa6c40910354cf4ee8";
      hash = "sha256-Bw/uysv6SbTmfwg/uSBga+wQRD+tEB/3f6FqE02Wqeg=";
    };
    files = ''("lisp/*.el")'';
    postPatch = ''
      substitute lisp/trampver.el.in lisp/trampver.el \
        --replace-fail '@configure_input@' 'Generated for the tramp-rpc Nix package' \
        --replace-fail '@PACKAGE_VERSION@' '${trampVersion}' \
        --replace-fail '@PACKAGE_BUGREPORT@' 'tramp-devel@gnu.org' \
        --replace-fail '@EMACS_REQUIRED_VERSION@' '28.1' \
        --replace-fail '@PACKAGE_URL@' 'https://www.gnu.org/software/tramp/' \
        --replace-fail '@TRAMP_EMACS_VERSION_CHECK@' '"ok"'
    '';
    packageRequires = [ ];
  };

  serverPackages = map (arch: {
    package = arch.callPackage "${package_src}/default.nix" { };
    system = arch.stdenv.hostPlatform.system;
  }) (with pkgsCross; [ musl64 aarch64-multiplatform-musl ]);
in melpaBuild rec {
  pname = "tramp-rpc";
  version = elispFileVersion "${package_src}/lisp/tramp-rpc.el";
  src = package_src;
  files = ''("lisp/*")'';

  postInstall = lib.concatMapStringsSep "\n" (server: ''
    install -m755 -D ${server.package}/bin/tramp-rpc-server $out/share/emacs/site-lisp/elpa/${pname}-${version}/binaries/${server.system}/tramp-rpc-server
  '') serverPackages;

  packageRequires = [ trampForTrampRpc msgpack ];
}
