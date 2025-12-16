{ mkDerivation, base, containers, lib, megaparsec, mtl, text }:
mkDerivation {
  pname = "scheme";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers megaparsec mtl text ];
  license = lib.licenses.bsd3;
  mainProgram = "scheme";
  homepage = "https://github.com/yaoshiu/scheme";
  description = "a toy scheme interpreter";
  maintainers = [
    {
      name = "Fay Ash";
      github = "yaoshiu";
      githubId = 56054933;
    }
  ];
}
