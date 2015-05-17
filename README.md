Gipeda -- the Git Performance Dashboard
=======================================

What is gipeda?
---------------

Gitpeda is a a tool that presents data from your program’s benchmark suite (or
any other source), with nice tables and shiny graphs.


It is only a frontend and does not help with or care about collecting the data.
So it is up to you whether you have a polling shell script loop, a post-commit
hook or a elaborate jenkins setup. As long as the performance data ends up in
the `logs/` directory, gipeda is happy.

Gipeda produces static pages. In fact, the (single) html file and the
accompagning JavaScript code is completely static. Giepda just generates a
large number of json files. This has the advantage of easy deployment: Just put
gipeda in your webspace of copy the files to some static web hosting and you
are done. This putts very little load on your server, is cache friendly and has
no security problems.

Do you want to see it live? Check out these:

 * [Demo page], visualizing fairly boring stuff about gipedia itself.
 * [GHC’s gipeda installation].

[Demo page]: http://nomeata.github.io/gipeda/
[GHC’s gipeda installation]: https://perf.haskell.org/ghc

Setting it up
-------------

 * Clone gipedia somewhere, possibly directly into your webspace.
 * Install a Haskell compiler, including the `cablal` tool.
 * Install a few packages

        apt-get install git unzip libfile-slurp-perl libipc-run-perl

 * Install the dependencies:

        cabal install --only-dependencies

 * Compile it:

        cabal install --bindir=.

 * Create a `settings.yaml`. You can look at the example file.
 * Clone the repository of your project into `repository/`. A bare clone is
   sufficient, e.g.

        git clone  --bare git://git.haskell.org/ghc.git repository

 * Download a bunch of JavaScript libraries by runing `./install-jslibs.sh`.	

Gipeda does not work without at least some logs, so lets add them.

Adding data
-----------

Gipeda expect simple CSV files for each revision, of the form

    benchmark1;1000
    benchmark2;20.123
    benchmark3;0

But likely your benchmark suite does not generate them in this format directly.
Hence, put whatever format you have (text base logs, JUnit reports, whatever)
into the directory `logs`, named `<gitrev>.log`, e.g.
`logs/0279a7d327a3b962ffa93a95d47ea5d9ee31e25c.log`.

Then create a script `log2csv` that expects the filename of such a log on on
the command line and produces the desired CSV file.

Running gipeda
--------------

With everything in place, you can now run

    ./gipeda

and it will create a bunch of JSON files in `site/out/`.  With `./gipda -j4`
you can parallize it.

You should do this everytime a new log file appears in `logs/`. You should also
make sure your repository is up-to-date, e.g. by running `git -C repository
pull` or, if it is a bare clone, `git -C repository fetch origin
"+refs/heads/*:refs/heads/*" --prune`.

Using gipedia
-------------

Finally, you simply point your browser to the `site/index.html`. The page
should be mostly self-explanatory. If you don’t see anything, it might be
because of the filter in the top-right corner. Try to enable all buttons, even
the `=`.

To host this on a webserver, just put the `site/` directory in your webspace.

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
