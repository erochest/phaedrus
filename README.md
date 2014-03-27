
# `phaedrus`

This reads a corpus to TEI document fragments, extracts the text from them, and
chops them up into overlapping chunks of standard sizes.

If a spreadsheet listing the evidence is given, those sections are
pulled---along with a number of non-marked chunks---and they are extracted into
a test/training set.

## Installation

To install this, just download it and use `cabal`:

```bash
cd phaedrus
cabal install
```

If you have a ZIP file of the binary release, change into `/usr/local` and
extract it there:

```bash
cd /usr/local
unzip ~/Downloads/phaedrus-0.1.0.0.zip
```

## Usage

Asking `phaedrus` for help generates this message (cleaned up and reformatted
somewhat).

> phaedrus v0.1.0.0: dialogue dataset generator
>
> Usage: phaedrus [-V|--version] [-d|--data-dir ARG] (-o|--output-dir ARG)
>                 [-D|--division ARG] [-w|--window ARG] [-O|--offset ARG]
>   Generate a training/test/dataset from the Dialogues.
>
> Available options:
>   -h,--help                Show this help text
>   -V,--version             Display the version information.
>   -d,--data-dir ARG        The directory for the data
>                            (default = /usr/local/share/x86_64-osx-ghc-7.6.3/phaedrus-0.1.0.0/data).
>   -o,--output-dir ARG      The output directory.
>   -D,--division ARG        How to divide the document. One of [D]ocument,
>                            [Se]ction, [P]age, or [Sp]eaking. (The brackets
>                            represent an abbreviation for that option.
>                            Default is 'Document.')
>   -w,--window ARG          The size of the window for the output chunks
>                            (default = 500).
>   -O,--offset ARG          The offset between the beginning of each window
>                            (default = 250).
>   -e,--evidence EVIDENCE-FILE The optional CSV file listing the
>                            evidentiary sections. Each line lists the
>                            dialogue title and the section (using the Latin
>                            alphabet).
>   -t,--training-size TRAINING-SIZE The default size for the training set,
>                            including both evidence and non-evidence groups.
>                            Default is 100.
>   -r,--evidence-ratio EVIDENCE-RATIO The amount of the training set to
>                            devote to evidence. For example, if
>                            TRAINING-SIZE is 100 and EVIDENCE-RATIO is 0.3,
>                            this will try to have 30 examples of evidence
>                            in the training set. if there's not enough to
>                            do this, then the training size is decreased.
>                            Default is 0.5.

We'll go into these options individually in more detail below as we discuss the
tasks that `phaedrus` can perform.

## Generating Chunks

The primary purpose of `phaedrus` is to generate overlapping chunks of text.
There are a number of options to do this.

### Input Data Directory

First, you can specify the input files to parse. They are also included in the
ZIP file and placed in a directory where `phaedrus` will look for it. However,
if the program has trouble finding the files or your move them, you can specify
their location with the `-d` or `--data-dir` options.

> phaedrus --data-dir /somewhere/else/data

### Output Directory

You **must** specify the location of the output directory. `phaedrus` will
create a number of subdirectories in this containing the chunked data files as
well as the evidence and non-evidence directories for the test- and
training-sets.

> phaedrus --output ./large-chunks

### Chopping up Documents

When the documents are chopped up, there are splits into overlapping windows
with the same number of tokens in each window. You can change both the window
size and the overlap.

The window size is changed using the `-w` or `--window` parameters. This
defaults to `500`.

The overlap is changed using the `-O` or `--offset` parameters. This defaults
to `250`.

> phaedrus --window 100 --offset 25

You can also change the largest level of container that gets split up. You do
this with the `-D` or `--division` parameters. Here are the options for this:

* **Document** This is the default. The text for one dialogue is split into
  chunks. Give the `d` or `document` values for this.
* **Section** This divides each dialogue into sections and then splits each
  section individually into chunks. Give the `se` or `section` values for this.
* **Page** This divides each dialogue into pages and then splits each page
  individually into chunks. Give the `p` or `page` values for this.
* **Speaker** This one is a little different. This takes each dialogue and
  gathers all the text for each speaker. It then splits the text for each
  speaker into chunks. Give the `sp` or `speaker` values for this.

Here are some examples of different calls using this:

> phaedrus --division document
> phaedrus --division section
> phaedrus --division page
> phaedrus --division speaker

## Generating Test/Training Sets

If you provide `phaedrus` with the information it needs, it will also generate
test- and training-sets for you. You just need to specify which sections are
interesting (i.e., *evidence*), and it will put all the chunks containing text
from that section into a folder. It will also randomly select a set of chunks
that do not contain any of those sections and put them in a separate folder.

### Evidence File

To specify which sections are "evidence," create a spreadsheet or CSV file with
two columns. The first column should contain the name of the dialogue in Greek.
The second column should contain the section in Latin (i.e., use *a* not *α*).

There should be *no* header row.

For example:

```csv
Ἀλκιβιάδης β, 138b
Ἀλκιβιάδης α, 109c
Χαρμίδης, 167e
Φαῖδρος, 231b
Φαῖδρος, 267d
Παρμενίδης, 158a
Κρατύλος, 389d
Κρατύλος, 405b
Μένων, 74b
Μένων, 90d
Μένων, 100c
```

To specify the evidence file, use the `-e` or `--evidence` parameters:

> phaedrus --evidence evidence-file-name.csv

### Training Size

You can specify the maximum number of documents to include in the
test-/training-set by using the `-t` or `--training-size` parameters. It
defaults to `100`.

> phaedrus --training-size 4000

### Evidence Ratio

You can also specify how much of the test- and training-sets are comprised of
evidence chunks using the `-r` and `--evidence-ratio` parameters. This defaults
to `0.3`, which means that evidence chunks will make up 30% of the
training-set.

> phaedrus --evidence-ratio 0.5

### The Actual Training Set Size

The `--training-size` and `--evidence-ratio` both contribute to determining the
actual size of the training set. The evidence ratio takes precedence, and the
training size is the maximum.

Let's work out some scenarios to see how these interact. In all of these cases,
we'll using the default values for the `--training-size` (100) and
`--evidence-ratio` (0.3), and the total size of the number of chunks that we
draw from will be large, say 1000.

* If there are a total of 50 chunks with evidence, then 30 of them are randomly
  selected and 70 non-evidence chunks are randomly selected. This respects the
  value of the `--training-size` by limiting the total size of the training
  set.
* If there are only 15 chunks with evidence, then they are all selected and
  only 50 non-evidence chunks are randomly selected. This respects the value of
  the `--evidence-ratio` by using that value to determine how large the
  training set should be.

## Get involved!

Please report bugs via the [github issue
tracker](https://github.com/scholarslab/NeatlineFeatures/issues).

Master github repository:

    git clone git@github.com:erochest/phaedrus.git
