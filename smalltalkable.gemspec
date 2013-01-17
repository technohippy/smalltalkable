# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)
require "smalltalkable/version"

Gem::Specification.new do |s|
  s.name        = "smalltalkable"
  s.version     = Smalltalkable::VERSION
  s.authors     = ["Ando Yasushi"]
  s.email       = ["andyjpn@gmail.com"]
  s.homepage    = "https://github.com/technohippy/smalltalkable"
  s.summary     = %q{Write your ruby code in smalltalkish way}
  s.description = %q{Smalltalkable is a library to write your ruby code in smalltalkish way.}

  s.rubyforge_project = "smalltalkable"

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]
end
