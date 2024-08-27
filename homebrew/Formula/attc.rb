class Attc < Formula
  desc "Tool for batch-converting attachments to preservation formats"
  homepage "https://dldc.lib.uchicago.edu/open/attachment-converter/index.html"
  url "https://github.com/uchicago-library/attachment-converter/archive/refs/tags/VER_NUM.tar.gz"
  sha256 ""
  license "GPL-2.0"

  depends_on "opam" => :build
  depends_on "ghostscript"
  depends_on "mercurial"
  depends_on "pandoc"
  depends_on "verapdf"
  depends_on "vips"

  def install
    ENV.deparallelize
    system "make", "pkg-build"
    bin.install "_build/default/main.exe" => "attc"
    lib.install Dir["conversion-scripts/*"]
  end

  def caveats
    <<~EOS
      Attachment Converter depends on LibreOffice for some functionality.

      Please install it using:
        brew install --cask libreoffice
    EOS
  end

  test do
    system "true"
  end
end
