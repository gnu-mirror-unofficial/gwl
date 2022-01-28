;; PiGx RNAseq Pipeline.
;; 
;; Copyright © 2017, 2018 Bora Uyar <bora.uyar@mdc-berlin.de>
;; Copyright © 2017, 2018 Jonathan Ronen <yablee@gmail.com>
;; Copyright © 2017-2021 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;; 
;; This file is part of the PiGx RNAseq Pipeline.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; TODO: validate configuration file!
;; include: string-append (config['locations']['pkglibexecdir'], 'scripts/validate_input.py')
;; validate_config(config)

require-packages
  . "guile"
  . "guile-dsv"     ; for CSV file parsing
  . "guile-libyaml" ; for YAML file parsing

import
  ice-9 match
  yaml
  dsv

define : load-settings
  define here
    or
      getenv "srcdir"
      getcwd

  define defaults
    file here / "etc" / "settings.yaml"

  unless : file-exists? defaults
    error "Could not find default settings!"

  let*
    :
      user-settings
        if : file-exists? "settings.yaml"
          read-yaml-file "settings.yaml"
          list
      ;; TODO: this is wrong
      settings
        append
          read-yaml-file defaults
          . user-settings
      extra-locations
        `
          :
            "prefix" . "PREFIX"
            "exec_prefix" . "EXEC_PREFIX"
            "libexecdir"  . "LIBEXECDIR"
            "pkglibexecdir" . "PKGLIBEXECDIR"
            "datarootdir" . "DATAROOTDIR"
            "pkgdatadir"   . "PKGDATADIR"
      ;; TODO: this is wrong
      new-settings
        append
          alist-delete "locations" settings
          append
            get settings "locations"
            . extra-locations

    . settings

    ;; settings['execution']['target'] = args.target
    ;; settings['locations'].update(dirs['locations'])

    ;; # Resolve relative paths in the locations section
    ;; root = path.dirname(sample_sheet)

    ;; for key in settings['locations']:
    ;;     settings['locations'][key] = path.normpath(path.join(here, root, settings['locations'][key]))

    ;; # Record the location of the sample sheet.
    ;; settings['locations']['sample-sheet'] = path.abspath(sample_sheet)

define config
  load-settings


;;; Locations

define GTF_FILE
  file (getcwd) /
    get config "locations" "gtf-file"

define SAMPLE_SHEET_FILE
  . "sample_sheet.csv"
  ; get config "locations" "sample-sheet"

define GENOME_FASTA
  get config "locations" "genome-fasta"

define CDNA_FASTA
  get config "locations" "cdna-fasta"

define READS_DIR
  get config "locations" "reads-dir"

define OUTPUT_DIR
  file (getcwd) /
    get config "locations" "output-dir"

define LOGO
  if : getenv "PIGX_UNINSTALLED"
    file
      ;; TODO
      ;;get config "locations" "pkgdatadir"
      getcwd
      . / "images" / "Logo_PiGx.png"
    file
      getcwd
      ;; TODO
      ;; get config "locations" "pkgdatadir"
      . / "Logo_PiGx.png"

define SCRIPTS_DIR
  file
    ;; TODO
    ;; get config "locations" "pkglibexecdir"
    getcwd
    . / "scripts"

define TRIMMED_READS_DIR
  file OUTPUT_DIR / "trimmed_reads"

define LOG_DIR
  file OUTPUT_DIR / "logs"

define FASTQC_DIR
  file OUTPUT_DIR / "fastqc"

define MULTIQC_DIR
  file OUTPUT_DIR / "multiqc"

define MAPPED_READS_DIR
  file OUTPUT_DIR / "mapped_reads"

define BIGWIG_DIR
  file OUTPUT_DIR / "bigwig_files"

define COUNTS_DIR
  file OUTPUT_DIR / "feature_counts"

define SALMON_DIR
  file OUTPUT_DIR / "salmon_output"


;;; Tools

define : toolArgs name
  get config default: "" "tools" name "args"

define : tool name
  string-append
    basename : get config "tools" name "executable"
    . " "
    toolArgs name

define FASTQC_EXEC
  tool "fastqc"
define MULTIQC_EXEC
  tool "multiqc"
define STAR_EXEC_MAP
  tool "star_map"
define STAR_EXEC_INDEX
  tool "star_index"
define SALMON_INDEX_EXEC
  tool "salmon_index"
define SALMON_QUANT_EXEC
  tool "salmon_quant"
define TRIM_GALORE_EXEC
  tool "trim-galore"
define SAMTOOLS_EXEC
  tool "samtools"
define HTSEQ_COUNT_EXEC
  tool "htseq-count"
define GUNZIP_EXEC
  tool "gunzip"
define RSCRIPT_EXEC
  tool "Rscript"
define SED_EXEC
  tool "sed"


;;; Configurations

define STAR_INDEX_THREADS
  get config "execution" "rules" "star_index" "threads"

define SALMON_INDEX_THREADS
  get config "execution" "rules" "salmon_index" "threads"

define STAR_MAP_THREADS
  get config "execution" "rules" "star_map" "threads"

define SALMON_QUANT_THREADS
  get config "execution" "rules" "salmon_quant" "threads"

define ORGANISM
  get config "organism"

define DE_ANALYSIS_LIST
  get config default: (list) "DEanalyses"

define : parse-sample-sheet file
  . "Parse FILE as a sample sheet and return a list (rows) of
lists (columns)."
  catch 'dsv-parser-error
    lambda _
      with-input-from-file file
        lambda _
          dsv->scm format: 'rfc4180
    lambda _
      error "Failed to parse the sample sheet."

;; This is a list of alists.  Each list item corresponds to a row.  A
;; row is an alist of column names to column values.
define SAMPLE_SHEET
  let
    :
    define all-rows
      parse-sample-sheet SAMPLE_SHEET_FILE
    define header
      first all-rows
    define rows
      drop all-rows 1
    map
      lambda : row
        map cons header row
      . rows

define SAMPLES
  on SAMPLE_SHEET map
    lambda : row
      get row "name"

define : lookup column predicate . fields
  . "Convenience function to access fields of sample sheet
columns that match the predicate.  The predicate may be a string.
This returns a list of rows matching the predicates, each
consisting of a list of selected fields.  If only one field was
selected, return just a list of matching columns."
  define predicate*
    if : procedure? predicate
      . predicate
      lambda : row
        equal? predicate : assoc-ref row column
  define records
    filter
      lambda : row
        predicate* row
      . SAMPLE_SHEET
  define return-fields
    match fields
      ;; Return list of lists
      : one more . rest
        lambda : record
          map
            lambda : column-name
              assoc-ref record column-name
            . fields
      ;; Return list of single columns
      : one
        lambda : record
          assoc-ref record : first fields

  ;; Return only the selected fields
  map return-fields records

define : reads-for-sample sample
  remove string-null?
    first
      lookup "name" sample "reads" "reads2"

define : single-end? sample
  . "Return #true if the SAMPLE library is single ended."
  equal? 1
    length : reads-for-sample sample

define : trimmed-reads-for-sample sample
  if : single-end? sample
    files TRIMMED_READS_DIR / sample "_R.fastq.gz"
    files TRIMMED_READS_DIR / sample "_R" (list "1" "2") ".fastq.gz"

define : trim-galore-input sample
  files READS_DIR /
    reads-for-sample sample


;;; Processes

process translate_sample_sheet_for_report
  inputs
    . sample-sheet: SAMPLE_SHEET_FILE
    . script:
    file SCRIPTS_DIR / "translate_sample_sheet_for_report.R"
  outputs
    file OUTPUT_DIR / "colData.tsv"
  packages
    . "r-minimal"
    . "coreutils" ; for rm
  # {
    {{RSCRIPT_EXEC}} {{inputs:script}} {{inputs:sample-sheet}} {{outputs}}
  }

process trim_galore_pe (with sample)
  inputs
    trim-galore-input sample
  outputs
    . r1:
    file TRIMMED_READS_DIR / sample "_R1.fastq.gz"
    . r2:
    file TRIMMED_READS_DIR / sample "_R2.fastq.gz"
    . log:
    file LOG_DIR / "trim_galore_" sample ".log"
  packages
    . "trim-galore"
    . "coreutils"
  values
    define read1
      first
        lookup "name" sample "reads"
    define read2
      first
        lookup "name" sample "reads2"
    ;; TODO: use basename instead?  Or is the extension not always the same?
    ;; For example ".fastq.gz" and ".fq.gz"?
    define : without-extension file-name
      string-join
        drop-right
          string-split file-name #\.
          . 2
        . "."
    list
      . tmp1:
      file TRIMMED_READS_DIR /
        without-extension read1
        . "_val_1.fq.gz"
      . tmp2:
      file TRIMMED_READS_DIR /
        without-extension read2
        . "_val_2.fq.gz"
  # {
    mkdir -p {{LOG_DIR}}
    {{TRIM_GALORE_EXEC}} -o {{TRIMMED_READS_DIR}} \
      --paired {{inputs}} >> {{outputs:log}} 2>&1 && \
      mv {{values:tmp1}} {{outputs:r1}} && \
      mv {{values:tmp2}} {{outputs:r2}}
  }

process trim_galore_se (with sample)
  inputs
    trim-galore-input sample
  outputs
    . trimmed:
    file TRIMMED_READS_DIR / sample "_R.fastq.gz"
    . log:
    file LOG_DIR / "trim_galore_" sample ".log"
  packages
    . "trim-galore"
    . "coreutils"
  values
    define read-name
      first
        lookup "name" sample "reads"
    ;; TODO: use basename instead?  Or is the extension not always the same?
    ;; For example ".fastq.gz" and ".fq.gz"?
    define without-extension
      string-join
        drop-right
          string-split read-name #\.
          . 2
        . "."
    list
      . tmp:
      file TRIMMED_READS_DIR / without-extension "_trimmed.fq.gz"
      . input1:
      first inputs
  # {
    mkdir -p {{LOG_DIR}}
    {{TRIM_GALORE_EXEC}} -o {{TRIMMED_READS_DIR}} \
      {{values:input1}} >> {{outputs:log}} 2>&1 && \
      mv {{values:tmp}} {{outputs:trimmed}}
  }

process star_index
  inputs
    . gtf: GTF_FILE
    . genome: GENOME_FASTA
  outputs
    . star_index_file:
    file OUTPUT_DIR / "star_index" / "SAindex"
    . star_index_dir:
    file OUTPUT_DIR / "star_index"
    . log:
    file LOG_DIR / "star_index.log"
  packages
    . "star"
    . "coreutils"
  # {
    mkdir -p {{LOG_DIR}}
    mkdir -p {{outputs:star_index_dir}}
    {{STAR_EXEC_INDEX}} \
      --runMode genomeGenerate \
      --runThreadN {{STAR_INDEX_THREADS}} \
      --genomeDir {{outputs:star_index_dir}} \
      --genomeFastaFiles {{inputs:genome}} \
      --sjdbGTFfile {{inputs:gtf}} >> {{outputs:log}} 2>&1
  }

process star_map (with sample)
  inputs
    ;; This process really depends on the whole directory (see
    ;; params.index_dir), but we can't register it as an input/output
    ;; in its own right since Snakemake 5.
    . index_file:
    pick star_index_file:
      process-outputs star_index with-tags:
    . index_dir:
    pick star_index_dir:
      process-outputs star_index with-tags:
    . reads:
    trimmed-reads-for-sample sample
  outputs
    file MAPPED_READS_DIR / sample "_Aligned.out.bam"
    . log:
    file LOG_DIR / "star_map_" sample ".log"
  packages
    . "star"
    . "gzip"
    . "coreutils"
  values
    . output_prefix:
    file MAPPED_READS_DIR / sample "_"
  # {
    mkdir -p {{LOG_DIR}}
    mkdir -p {{MAPPED_READS_DIR}}
    {{STAR_EXEC_MAP}}                         \
      --runThreadN {{STAR_MAP_THREADS}}       \
      --genomeDir {{inputs:index_dir}}        \
      --readFilesIn {{inputs::reads}}         \
      --readFilesCommand '{{GUNZIP_EXEC}} -c' \
      --outSAMtype BAM Unsorted               \
      --outFileNamePrefix {{values:output_prefix}} >> {{outputs:log}} 2>&1
  }

process sort_bam (with sample)
  inputs
    file MAPPED_READS_DIR / sample "_Aligned.out.bam"
  outputs
    . bam:
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam"
    . log:
    file LOG_DIR / "samtools_sort_" sample ".log"
  packages
    . "samtools"
    . "coreutils"
  # {
    mkdir -p {{LOG_DIR}}
    {{SAMTOOLS_EXEC}} sort -o {{outputs:bam}} {{inputs}} >> {{outputs:log}} 2>&1
  }

process index_bam (with sample)
  inputs
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam"
  outputs
    . bai:
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam.bai"
    . log:
    file LOG_DIR / "samtools_index_" sample ".log"
  packages
    . "samtools"
    . "coreutils"
  # {
    mkdir -p {{LOG_DIR}}
    {{SAMTOOLS_EXEC}} index {{inputs}} {{outputs:bai}} >> {{outputs:log}} 2>&1
  }

process fastqc (with sample)
  inputs
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam"
  outputs
    file FASTQC_DIR / sample "_Aligned.sortedByCoord.out_fastqc.zip"
    . log:
    file LOG_DIR / "fastqc_" sample ".log"
  packages
    . "fastqc"
    . "coreutils"
  # {
    mkdir -p {{LOG_DIR}}
    mkdir -p {{FASTQC_DIR}}
    {{FASTQC_EXEC}} -o {{FASTQC_DIR}} -f bam {{inputs}} >> {{outputs:log}} 2>&1
  }

;; TODO: this fails in a container:
;; Executing: /bin/sh -c /gnu/store/c0kw3nz6857rhya6klx53kd818jiim0c-gwl-salmon_index-container.scm 
;; Backtrace:
;;            5 (primitive-load "/gnu/store/c0kw3nz6857rhya6klx53kd818j?")
;; In gnu/build/linux-container.scm:
;;     299:8  4 (call-with-temporary-directory #<procedure 7f0829d14e60?>)
;;    327:16  3 (_ _)
;;    264:16  2 (run-container _ _ (mnt pid ipc uts user net) _ #<proc?> ?)
;; In ice-9/boot-9.scm:
;;    260:13  1 (for-each #<procedure 7f0829db56c0 at ice-9/eval.scm:3?> ?)
;; In unknown file:
;;            0 (copy-file "./output/salmon_index/pos.bin" "/gwl/./outpu?")

;; ERROR: In procedure copy-file:
;; In procedure copy-file: Not a directory
;; error: process `salmon_index' failed to produce output ./output/salmon_index/pos.bin.
process salmon_index
  inputs
    . CDNA_FASTA
  outputs
    . salmon_index_dir:
    file OUTPUT_DIR / "salmon_index"
    . salmon_index_file:
    file OUTPUT_DIR / "salmon_index" / "pos.bin"
    . log:
    file LOG_DIR / "salmon_index.log"
  packages
    . "salmon"
    . "coreutils"
  # {
    mkdir -p {{LOG_DIR}}
    mkdir -p {{outputs:salmon_index_dir}}
    {{SALMON_INDEX_EXEC}} -t {{inputs}} \
      -i {{outputs:salmon_index_dir}}   \
      -p {{SALMON_INDEX_THREADS}} >> {{outputs:log}} 2>&1
  }

;; This process really depends on the whole directory, not just the index_file (see
;; params.index_dir), but we can't register it as an input/output
;; in its own right since Snakemake 5.
process salmon_quant (with sample)
  inputs
    . gtf: GTF_FILE
    . index_file:
    pick salmon_index_file:
      process-outputs salmon_index with-tags:
    . index_dir:
    pick salmon_index_dir:
      process-outputs salmon_index with-tags:
    . reads:
    trimmed-reads-for-sample sample
  outputs
    file SALMON_DIR / sample / "quant.sf"
    file SALMON_DIR / sample / "quant.genes.sf"
    . outfolder:
    file SALMON_DIR / sample
    . log:
    file LOG_DIR / "salmon_quant_" sample ".log"
  packages
    . "salmon"
    . "coreutils"
  values
    define reads
      pick * reads: inputs
    define single?
      equal? 1
        length reads
    list
      . arguments:
      if single?
        string-append "-r " (first reads) " "
        string-append "-1 " (first reads) " -2 " (second reads) " "
  # {
    mkdir -p {{LOG_DIR}}
    {{SALMON_QUANT_EXEC}}              \
      -i {{inputs:index_dir}}          \
      -l A -p {{SALMON_QUANT_THREADS}} \
      {{values:arguments}}             \
      -o {{outputs:outfolder}}         \
      --seqBias --gcBias               \
      -g {{inputs:gtf}} >> {{outputs:log}} 2>&1
  }

process counts_from_SALMON
  inputs
    . quantFiles:
    files SALMON_DIR / SAMPLES / "quant.sf"
    . quantGenesFiles:
    files SALMON_DIR / SAMPLES / "quant.genes.sf"
    . colDataFile:
    first
      process-outputs translate_sample_sheet_for_report
    . script:
    file SCRIPTS_DIR / "counts_matrix_from_SALMON.R"
  outputs
    define types
      list "transcripts" "genes"
    append
      files COUNTS_DIR / "raw_counts" / "counts_from_SALMON." types ".tsv"
      files COUNTS_DIR / "normalized" / "TPM_counts_from_SALMON." types ".tsv"
      list log:
        file LOG_DIR / "salmon_import_counts.log"
      cons directories:
        files COUNTS_DIR / (list "raw_counts" "normalized")
  packages
    . "r-minimal"
    . "r-data-table"
    . "r-tximport"
    . "r-deseq2"
    . "coreutils"
  # {
    mkdir -p {{LOG_DIR}}
    mkdir -p {{outputs::directories}}
    {{RSCRIPT_EXEC}} {{inputs:script}} \
      {{SALMON_DIR}} {{COUNTS_DIR}} {{inputs:colDataFile}} >> {{outputs:log}} 2>&1
  }

process genomeCoverage (with sample)
  inputs
    . size_factors_file:
    file COUNTS_DIR / "normalized" / "deseq_size_factors.txt"
    . bam:
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam"
    . bai:
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam.bai"
    . script:
    file SCRIPTS_DIR / "export_bigwig.R"
  outputs
    files BIGWIG_DIR / sample (list ".forward" ".reverse") ".bigwig"
    . log:
    file LOG_DIR / "genomeCoverage_" sample ".log"
  packages
    . "r-minimal"
    . "r-s4vectors"
    . "r-genomicalignments"
    . "r-genomicranges"
    . "r-rtracklayer"
    . "coreutils"
  values
    . sample: sample
  # {
    mkdir -p {{LOG_DIR}}
    mkdir -p {{BIGWIG_DIR}}
    {{RSCRIPT_EXEC}} {{inputs:script}} \
      {{inputs:bam}} {{values:sample}} {{inputs:size_factors_file}} {{BIGWIG_DIR}} >> {{outputs:log}} 2>&1
  }      

process multiqc
  inputs
    . salmon_output:
    files SALMON_DIR / SAMPLES / "quant.sf"
    . star_output:
    files MAPPED_READS_DIR / SAMPLES "_Aligned.sortedByCoord.out.bam"
    . fastqc_output:
    files FASTQC_DIR / SAMPLES "_Aligned.sortedByCoord.out_fastqc.zip"
  outputs
    file MULTIQC_DIR / "multiqc_report.html"
    . log:
    file LOG_DIR / "multiqc.log"
  packages
    . "bash"
    . "multiqc"
    . "coreutils"
  # bash {
    mkdir -p {{LOG_DIR}}
    mkdir -p /tmp/multiqc-temp
    # oops: {{inputs}} includes tags!
    for file in {{inputs::salmon_output}} {{inputs::star_output}} {{inputs::fastqc_output}}; do
      dir="/tmp/multiqc-temp/$(dirname ${file})"
      mkdir -p ${dir}
      ln -t ${dir} ${file}
    done
    {{MULTIQC_EXEC}} -o {{MULTIQC_DIR}} /tmp/multiqc-temp >> {{outputs:log}} 2>&1
  }
    
process count_reads (with sample)
  inputs
    . gtf: GTF_FILE
    . bam:
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam"
    . bai:
    file MAPPED_READS_DIR / sample "_Aligned.sortedByCoord.out.bam.bai"
    . script:
    file SCRIPTS_DIR / "count_reads.R"
  outputs
    file MAPPED_READS_DIR / sample ".read_counts.csv"
    . log:
    file LOG_DIR / sample ".count_reads.log"
  packages
    . "coreutils"
    . "r-minimal"
    . "r-genomicalignments"
    . "r-rsamtools"
    . "r-rtracklayer"
  values
    . sample: sample
    . single_end:
    if : single-end? sample
      . "TRUE" "FALSE"
    . mode:
    get config "counting" "counting_mode"
    . nonunique:
    get config "counting" "count_nonunique"
    . strandedness:
    get config "counting" "strandedness"
    . feature:
    get config "counting" "feature"
    . group_by:
    get config "counting" "group_feature_by"
    . yield_size:
    get config "counting" "yield_size"
  # {
    {{RSCRIPT_EXEC}} {{inputs:script}} \
      {{values:sample}} {{inputs:bam}} {{inputs:gtf}} \
      {{values:single_end}} {{values:mode}} {{values:nonunique}} {{values:strandedness}} \
      {{values:feature}} {{values:group_by}} {{values:yield_size}} >> {{outputs:log}} 2>&1
  }

process collate_read_counts
  inputs
    . counts:
    files MAPPED_READS_DIR / SAMPLES ".read_counts.csv"
    . script:
    file SCRIPTS_DIR / "collate_read_counts.R"
  outputs
    file COUNTS_DIR / "raw_counts" / "counts_from_star.tsv"
    . log:
    file LOG_DIR / "collate_read_counts.log"
  packages
    . "r-minimal"
    . "r-data-table"
    . "coreutils" ; for "rm"
  # {
    touch {{outputs:log}} # TODO: the script doesn't produce any log!
    {{RSCRIPT_EXEC}} {{inputs:script}} {{MAPPED_READS_DIR}} {{outputs}} >> {{outputs:log}} 2>&1
  }

process htseq_count
  inputs
    . gtf: GTF_FILE
    . bams:
    files MAPPED_READS_DIR / SAMPLES "_Aligned.sortedByCoord.out.bam"
  outputs
    . stats_file:
    file COUNTS_DIR / "raw_counts" / "htseq_stats.txt"
    . counts_file:
    file COUNTS_DIR / "raw_counts" / "counts_from_star_htseq-count.txt"
    . log:
    file LOG_DIR / "htseq-count.log"
  packages
    . "sed"
    . "htseq" ; htseq-count
    . "coreutils" ; head, tail, rm
  values
    . tmp_file:
    file COUNTS_DIR / "raw_counts" / "htseq_out.txt"
    . samples: SAMPLES
  # {
      echo {{values:samples}} | sed 's/ /\t/g' > {{values:tmp_file}}
      htseq-count {{inputs::bams}} {{inputs:gtf}} 1>> {{values:tmp_file}} 2>> {{outputs:log}}

      # move feature count stats (e.g. __no_feature etc) to another file
      echo {{values:samples}} > {{outputs:stats_file}}
      tail -n 5 {{values:tmp_file}} >> {{outputs:stats_file}}

      # only keep feature counts in the counts table (remove stats)
      head -n -5 {{values:tmp_file}} > {{outputs:counts_file}}

      # remove temp file
      rm {{values:tmp_file}}
  }

;; Create a normalized counts table including all samples using the
;; median-of-ratios normalization procedure of deseq2.
process norm_counts_deseq
  inputs
    . counts_file:
    file COUNTS_DIR / "raw_counts" / "counts_from_star.tsv"
    . colDataFile:
    first
      process-outputs translate_sample_sheet_for_report
    . script:
    file SCRIPTS_DIR / "norm_counts_deseq.R"
  outputs
    . outdir:
    file COUNTS_DIR / "normalized"
    . size_factors:
    file COUNTS_DIR / "normalized" / "deseq_size_factors.txt"
    . norm_counts:
    file COUNTS_DIR / "normalized" / "deseq_normalized_counts.tsv"
    . log:
    file LOG_DIR / "norm_counts_deseq.log"
  packages
    . "coreutils" ; for "rm"
    . "r-minimal"
    . "r-deseq2"
  # {
    {{RSCRIPT_EXEC}} {{inputs:script}} \
      {{inputs:counts_file}} \
      {{inputs:colDataFile}} \
      {{outputs:outdir}} >> {{outputs:log}} 2>&1
  }

process report (with type analysis)
  inputs
    . logo: LOGO
    . gtf: GTF_FILE
    . counts:
    file COUNTS_DIR / "raw_counts" / "counts_from_"
      . (case type
         ((star) "star")
         ((salmon-transcripts) "SALMON.transcripts")
         ((salmon-genes) "SALMON.genes"))
      . ".tsv" 
    . coldata:
    first
      process-outputs translate_sample_sheet_for_report
    . reportR:
    file SCRIPTS_DIR / "runDeseqReport.R"
    . reportRmd:
    file SCRIPTS_DIR / "deseqReport.Rmd"
  outputs
    . outdir:
    file OUTPUT_DIR / "report"
    file OUTPUT_DIR / "report" / analysis "."
      . (case type
         ((star) "star")
         ((salmon-transcripts) "salmon.transcripts")
         ((salmon-genes) "salmon.genes"))
      . ".deseq.report.html"
    . log:
    file LOG_DIR / analysis ".report."
      . (case type
         ((star) "star")
         ((salmon-transcripts) "salmon.transcripts")
         ((salmon-genes) "salmon.genes"))
      . ".log"
  packages
    . "coreutils"  ; for "rm"
    . "r-minimal"
    . "r-corrplot"
    . "r-crosstalk"
    . "r-deseq2"
    . "r-dt"
    . "r-ggplot2"
    . "r-ggrepel"
    . "r-gprofiler"
    . "r-knitr"
    . "r-pheatmap"
    . "r-plotly"
    . "r-reshape2"
    . "r-rmarkdown"
    . "r-rtracklayer"
    . "r-scales"
    . "r-summarizedexperiment"
  values
    . analysis: analysis
    . case:
    get DE_ANALYSIS_LIST analysis "case_sample_groups"
    . control:
    get DE_ANALYSIS_LIST analysis "control_sample_groups"
    . covariates:
    get DE_ANALYSIS_LIST analysis "covariates"
    . suffix: (case type
               ((star) "star")
               ((salmon-transcripts) "salmon.transcripts")
               ((salmon-genes) "salmon.genes"))
  # {
    {{RSCRIPT_EXEC}} {{inputs:reportR}} \
      --logo={{inputs:logo}} \
      --prefix='{{values:analysis}}.{{values:suffix}}' \
      --reportFile={{inputs:reportRmd}} \
      --countDataFile={{inputs:counts}} \
      --colDataFile={{inputs:coldata}} \
      --gtfFile={{inputs:gtf}} \
      --caseSampleGroups='{{values:case}}' \
      --controlSampleGroups='{{values:control}}' \
      --covariates='{{values:covariates}}' \
      --workdir={{outputs:outdir}} \
      --organism='{{ORGANISM}}' >> {{outputs:log}} 2>&1
  }


define ANALYSES
  map first DE_ANALYSIS_LIST
  
process final_report
  synopsis "Produce a comprehensive report.  This is the default target."
  inputs
    file OUTPUT_DIR / "star_index" / "SAindex"
    file OUTPUT_DIR / "salmon_index" / "pos.bin"
    file MULTIQC_DIR / "multiqc_report.html"
    file COUNTS_DIR / "raw_counts" / "counts_from_SALMON.transcripts.tsv"
    file COUNTS_DIR / "raw_counts" / "counts_from_SALMON.genes.tsv"
    file COUNTS_DIR / "normalized" / "TPM_counts_from_SALMON.transcripts.tsv"
    file COUNTS_DIR / "normalized" / "TPM_counts_from_SALMON.genes.tsv"
    file COUNTS_DIR / "raw_counts" / "counts_from_star.tsv"
    file COUNTS_DIR / "normalized" / "deseq_normalized_counts.tsv"
    file COUNTS_DIR / "normalized" / "deseq_size_factors.txt"

    files BIGWIG_DIR / SAMPLES ".forward.bigwig"
    files BIGWIG_DIR / SAMPLES ".reverse.bigwig"
    files OUTPUT_DIR / "report" / ANALYSES
      list ".star.deseq.report.html"
         . ".salmon.transcripts.deseq.report.html"
         . ".salmon.genes.deseq.report.html"
  procedure
    ' display "Final reports have been generated.\n"

process deseq_report_star
  synopsis "Produce one HTML report for each analysis based on STAR results."
  inputs
    files OUTPUT_DIR / "report" / ANALYSES ".star.deseq.report.html"
  procedure
    ' display "STAR reports have been generated.\n"

process deseq_report_salmon_transcripts
  synopsis "Produce one HTML report for each analysis based on SALMON results at transcript level."
  inputs
    files OUTPUT_DIR / "report" / ANALYSES ".salmon.transcripts.deseq.report.html"
  procedure
    ' display "SALMON transcript reports have been generated.\n"

process deseq_report_salmon_genes
  synopsis "Produce one HTML report for each analysis based on SALMON results at gene level."
  inputs
    files OUTPUT_DIR / "report" / ANALYSES ".salmon.genes.deseq.report.html"
  procedure
    ' display "SALMON genes reports have been generated.\n"

process star-map-done
  synopsis "Produce a STAR mapping results in BAM file format."
  inputs
    files MAPPED_READS_DIR / SAMPLES "_Aligned.sortedByCoord.out.bam"
  procedure
    ' display "STAR mappings have been generated.\n"

process star_counts
  synopsis "Get count matrix from STAR mapping results using summarizeOverlaps."
  inputs
    file COUNTS_DIR / "raw_counts" / "counts_from_star.tsv"
  procedure
    ' display "STAR count matrix has been generated.\n"

process genome_coverage
  synopsis "Compute genome coverage values from BAM files - save in bigwig format"
  inputs
    files BIGWIG_DIR / SAMPLES (list ".forward" ".reverse") ".bigwig"
  procedure
    ' display "Genome coverage has been computed.\n"

process fastqc-done
  synopsis "post-mapping quality control by FASTQC."
  inputs
    files FASTQC_DIR / SAMPLES "_Aligned.sortedByCoord.out_fastqc.zip"
  procedure
    ' display "FastQC quality control completed.\n"

process salmon-index-done
  synopsis "Create SALMON index file."
  inputs
    file OUTPUT_DIR / "salmon_index" / "pos.bin"
  procedure
    ' display "SALMON index has been generated.\n"

process salmon-quant-done
  synopsis "Calculate read counts per transcript using SALMON."
  inputs
    files SALMON_DIR / SAMPLES / (list "quant.sf" "quant.genes.sf")
  procedure
    ' display "SALMON read counts has been computed.\n"

process salmon-counts-done
  synopsis "Get count matrix from SALMON quant."
  inputs
    file COUNTS_DIR / "raw_counts" / "counts_from_SALMON.transcripts.tsv"
    file COUNTS_DIR / "raw_counts" / "counts_from_SALMON.genes.tsv"
    file COUNTS_DIR / "normalized" / "TPM_counts_from_SALMON.transcripts.tsv"
    file COUNTS_DIR / "normalized" / "TPM_counts_from_SALMON.genes.tsv"
  procedure
    ' display "SALMON count matrix has been computed.\n"

process multiqc-done
  synopsis "Get multiQC report based on STAR alignments and fastQC reports."
  inputs
    file MULTIQC_DIR / "multiqc_report.html"
  procedure
    ' display "multiQC report completed.\n"

;; TODO: allow selection of targets
;; # Selected output files from the above set.
;; selected_targets = config['execution']['target'] or ['final-report']

workflow rnaseq
  before
    lambda _
      display-file "etc/pretty.txt"
  after
    lambda _
      newline
      display "PiGx RNAseq analysis complete."
      newline
  processes
    apply auto-connect
      append
        ;; process templates using samples
        append-map
          lambda : sample
            list
              if : single-end? sample
                trim_galore_se sample
                trim_galore_pe sample
              star_map sample
              sort_bam sample
              index_bam sample
              fastqc sample
              salmon_quant sample
              genomeCoverage sample
              count_reads sample
          . SAMPLES

        ;; report process templates using analyses
        append-map
          lambda : analysis
            list
              report 'star analysis
              report 'salmon-transcripts analysis
              report 'salmon-genes analysis
          . ANALYSES
        ;; simple processes
        list
          . translate_sample_sheet_for_report
          . star_index
          . salmon_index
          . counts_from_SALMON
          . multiqc
          . collate_read_counts
          . htseq_count
          . norm_counts_deseq
        ;; public targets
        list
          . final_report
          . deseq_report_star
          . deseq_report_salmon_transcripts
          . deseq_report_salmon_genes
          . star-map-done
          . star_counts
          . genome_coverage
          . fastqc-done
          . salmon-index-done
          . salmon-quant-done
          . salmon-counts-done
          . multiqc-done
