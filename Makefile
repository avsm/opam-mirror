all:
	dune build src/opam_mirror_fetch_urls.exe src/opam_mirror_show_urls.exe

clean:
	dune clean
