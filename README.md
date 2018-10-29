Gipeda -- the Git Performance Dashboard
=======================================

What is gipeda?
---------------

Gipeda is a a tool that presents data from your program’s benchmark suite (or
any other source), with nice tables and shiny graphs.


It is only a frontend and does not help with or care about collecting the data.
So it is up to you whether you have a polling shell script loop, a post-commit
hook or a elaborate jenkins setup. As long as the performance data ends up in
the `logs/` directory, gipeda is happy.

Gipeda produces static pages. In fact, the (single) html file and the
accompagning JavaScript code is completely static. Gipeda just generates a
large number of json files. This has the advantage of easy deployment: Just put
gipeda in your webspace of copy the files to some static web hosting and you
are done. This putts very little load on your server, is cache friendly and has
no security problems.

Do you want to see it live? Check out these:

 * [Demo page], visualizing fairly boring stuff about gipeda itself.
 * [GHC’s gipeda installation].

[Demo page]: http://perf.haskell.org/gipeda
[GHC’s gipeda installation]: https://perf.haskell.org/ghc

Setting it up
-------------

 * Clone gipeda somewhere, possibly directly into your webspace.
 * Install a Haskell compiler, including the `cabal` tool.
 * Install a few packages

        apt-get install git unzip libssl-dev libfile-slurp-perl libipc-run-perl libicu-dev

 * Install the dependencies:

        cabal install --only-dependencies

 * Compile it:

        cabal install --bindir=.

 * Create a `gipeda.yaml`. You can look at the example file.
 * Clone the repository of your project into `repository/`. A bare clone is
   sufficient, e.g.

        git clone  --bare git://git.haskell.org/ghc.git repository

 * Download a bunch of JavaScript libraries by running `./install-jslibs.sh`.	

Gipeda does not work without at least some logs, so let's add them.

Adding data
-----------

Gipeda expects to find performance data in the `logs` directory, where the
data for each Git revision is named `<gitrev>.log`, e.g.
`logs/0279a7d327a3b962ffa93a95d47ea5d9ee31e25c.log`.

Handling data
-------------

### Criterion data

Gipeda has built-in functionality for handling Criterion output, if your
benchmarks use the Criterion framework.

Just put Criterion logs directly in the `logs` directory (named appropriately),
and copy the `binary/log2csv` script to the base directory of this repo, e.g.

    cp binary/log2csv .

### Custom or non-Criterion data

For non-Criterion data (text base logs, JUnit reports, or anything else), you
will need to make your own script `log2csv` and put it in the base directory of
this repo. This script should expect the filename of a log on the command line,
and should output a CSV file for that Git revision, of the form

    benchmark1;1000
    benchmark2;20.123
    benchmark3;0

Running gipeda
--------------

With everything in place, you can now run

    ./gipeda

and it will create a bunch of JSON files in `site/out/`.  With `./gipeda -j4`
you can parallelize it.

You should do this everytime a new log file appears in `logs/`. You should also
make sure your repository is up-to-date, e.g. by running `git -C repository
pull` or, if it is a bare clone, `git -C repository fetch origin
"+refs/heads/*:refs/heads/*" --prune`.

Using gipeda
-------------

Finally, you simply point your browser to the `site/index.html`. The page
should be mostly self-explanatory. If you don’t see anything, it might be
because of the filter in the top-right corner. Try to enable all buttons, even
the `=`.

To host this on a webserver, just put the `site/` directory in your webspace.

Hacking on gipeda
-----------------

Gipeda doesn't do much; it mostly assembles the data and creates nice reports.
The rough pipeline is as follows:

 * Directory `logs/` contains project-specific data per git commit that has
   been benchmarked. gipeda will run `log2csv` on these files to generate the
   files in `site/out/results`. `logs` may be a normal directory, or (for disk
   space efficiency) a bare git repository. This step is optional.
 * Directory `site/out/results` contains one csv file per git commit. The
   format is simple, as there are two columns: benchmark name and a numerical
   value.
 * From these files, gipeda generates a number of JSON files, some per commit
   (`report`, `summaries`), some global (`settings`, `latest-summaries`).

   A crucial idea here is that these JSON files are all but fragments of a
   theoretical global JSON document. In other words: You could combine them
   (using a naive JSON object merge) and there would be no conflicts, and the
   result could be used by the client as well.
 * The client (`site/index.html` and `site/js/gipeda.js`) is a fairly standard
   HTML+JS application using jquery, bootstrap, handlebars.

Bugs, Code, Contact
-------------------

Please reports bugs and missing features at the [GitHub bugtracker]. This is
also where you can find the [source code].

Gipeda was written by [Joachim Breitner] and is licensed under a permissive MIT
[license].

[GitHub bugtracker]: https://github.com/nomeata/gipeda/issues
[source code]: https://github.com/nomeata/gipeda
[Joachim Breitner]: http://www.joachim-breitner.de/
[license]: https://github.com/nomeata/gipeda/blob/LICENSE
