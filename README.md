# ytb-dl

Parallelize downloads of multiple streams from youtube. For example, use the url.txt file (located under test directory) to fetch all AI classes on ML and AI:
stack build
stack exec ytb-dl-exe -- -i test/url.txt -o output-dir
