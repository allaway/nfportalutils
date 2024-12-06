# HELPERS ----------------------------------------------------------------------#

#' Helper to process paths
#'
#' Expects paths of equal length
#' @keywords internal
path_extract <- function(paths, index_fun) {
  paths <- strsplit(paths, "/")
  index <- index_fun(paths[[1]])
  sapply(paths, `[`, index)
}


#' Match to output-specific annotation function
#'
#' This encodes logic for annotation and checks, e.g.
#' DeepVariant is Germline variant calling only, Mutect2 is for Somatic variant calling only,
#' while FreeBayes and Strelka2 can be applied to both; see
#' https://raw.githubusercontent.com/nf-core/sarek/3.4.2//docs/images/sarek_workflow.png
#'
#' @keywords internal
#'
annotation_rule <- function(outputFrom, which = c("format_as", "annotate_as", "template")) {

  which_fun <- match.arg(which)
  ref <- list(
    "STAR and Salmon" = list(format_as = function(x) { "sf" }, annotate_as = "annotate_quantified_expression", template = "bts:ProcessedExpressionTemplate"),
    "featureCounts" = list(format_as = function(x) { "txt" }, annotate_as =  "annotate_quantified_expression", template = "bts:ProcessedExpressionTemplate"),
    "SAMtools" = list(format_as = function(x) { substring(x, nchar(x)-2, nchar(x)) }, annotate_as = "annotate_aligned_reads", template = "bts:ProcessedAlignedReadsTemplate"),
    "CNVkit" = list(format_as = function(x) { substring(x, nchar(x)-2, nchar(x)) }, annotate_as = "annotate_called_variants", template = "bts:ProcessedVariantCallsTemplate"),
    "DeepVariant" = list(format_as = function(x) { "vcf" }, annotate_as = "annotate_called_variants", template = "bts:ProcessedVariantCallsTemplate"),
    "Strelka2" = list(format_as = function(x) { "vcf" }, annotate_as = "annotate_called_variants", template = "bts:ProcessedVariantCallsTemplate"),
    "Mutect2" = list(format_as = function(x) { "vcf" }, annotate_as = "annotate_called_variants", template = "bts:ProcessedVariantCallsTemplate"),
    "FreeBayes" = list(format_as = function(x) { "vcf" }, annotate_as = "annotate_called_variants", template = "bts:ProcessedVariantCallsTemplate"))

  switch(
    which,
    "format_as" = ref[[outputFrom]][[which]],
    "annotate_as" = match.fun(ref[[outputFrom]][[which]]),
    "template" = ref[[outputFrom]][[which]]
  )

}

# ------------------------------------------------------------------------------#

#' Parse nextflow samplesheet for sample inputs
#'
#' Samplesheets are used in rnaseq pipelines, defined here: https://nf-co.re/rnaseq/usage#full-samplesheet.
#' After the pipeline is run, it should be found in an output folder called `pipeline_info`.
#' This is a simple helper to get a mapping of sample ids to input files (either one-to-many or one-to-one) as a table.
#'
#' @param samplesheet A local file or syn id of samplesheet.
#' @param parse_fun Function implementing how to parse samples in samplesheet.
#' @import data.table
#' @export
map_sample_input_ss <- function(samplesheet,
                                parse_fun = function(x) gsub("_T[0-9]$", "", x)) {

  ss <- dt_read(samplesheet)
  # Annoyingly, headers are not standard and can use fastq or fastq1 instead of fastq_1
  if("fastq1" %in% names(ss)) {
    setnames(ss, c("fastq1", "fastq2"), c("fastq_1", "fastq_2"))
    message("In this samplesheet version, looks like we have 'fastq1' and 'fastq2' -- reading as 'fastq_1' and 'fastq_2")
  }
  if("fastq" %in% names(ss)) {
    setnames(ss, "fastq", "fastq_1")
    message("In this samplesheet version, looks like we have 'fastq' -- reading as 'fastq_1'")
  }

  ss[, input_syn_1 := bare_syn_id(fastq_1)] # Get synId from URI
  ss[, input_syn_2 := bare_syn_id(fastq_2)]
  ss[, sample := parse_fun(sample)] # Parse sample from "sample" col

  # File inputs for each sample specimen
  sample_inputs <- ss[, .(input_id = list(na.omit(c(input_syn_1, input_syn_2)))), by = sample]
  return(sample_inputs)
}


#' Map sample to output from nf-rnaseq with path
#'
#' See https://nf-co.re/rnaseq/docs/output/#pipeline-overview.
#' The workflow generates several types of outputs that can become ("level 2" or "level 3") dataset products.
#' (note: some outputs may not be available depending on how workflow was run and indexed back).
#'
#' @inheritParams map_sample_output_sarek
#' @param output Which output to select and annotate. Defaults to all output types unless more limited election.
#' - "STAR and Salmon" selects .sf files -- this is typically considered the main output.
#' - "featureCounts" selects the relevant .txt files.
#' - "SAMtools" selects the .bam/.bai indexed and sorted by SAMtools.
#' @return A list of `data.table`s with columns `output_name` `output_id` `sample` `workflow` for each output type.
#' An attribute `workflow=nf-rnaseq` will be set on the returned list,
#' and elements will have attribute `outputFrom` set, e.g. `outputFrom=SAMtools`.
#' @export
map_sample_output_rnaseq <- function(syn_out,
                                     fileview,
                                     output = c("STAR and Salmon", "featureCounts", "SAMtools")) {

  output <- match.arg(output, several.ok = T)

  path <- get_path(syn_out)
  message("Path: ", path)

  results <- list()
  for(.output in output) {
    # Note - featureCounts selects *.featureCounts.txt and *.featureCounts.txt.summary
    # and omits *_mqc.tsv since it's considered a custom content file
    path_spec <- switch(.output,
                        "STAR and Salmon" = glue::glue("path like '{path}%.sf'"),
                        "featureCounts" = glue::glue("path like '{path}/featureCounts/%.txt' or path like '{path}/featureCounts/%.txt.summary'"),
                        "SAMtools" = glue::glue("(path like '{path}/%.bam' or path like '{path}/%.bai')"))
    query <- glue::glue("SELECT path, name as output_name, id as output_id from {fileview} where {path_spec} and type = 'file'")
    result <- as.data.table(.syn$tableQuery(query)$asDataFrame())
    message("Found ", nrow(result), " files for ", .output)

    if(nrow(result)) {
      # `sample` parsed differently depending on output type;
      # STAR and Salmon relies on parent folder name while others have sample in filename
      if(.output == "STAR and Salmon") {
        result[, sample := path_extract(path, index_fun = function(x) length(x) - 1)]
      }

      if(.output == "featureCounts") {
        result[, sample := gsub("\\.(txt|featureCounts).*$", "", output_name)]
      }

      if(.output == "SAMtools") {
        result[, sample := gsub("\\.markdup.*", "", output_name)]
      }
    }

    result[, workflow := "STAR and Salmon"]
    results[[.output]] <- result
    setattr(results[[.output]], "outputFrom", .output)  # annotation within an annotation workflow...enable reasoning in later steps
  }

  # results <- rbindlist(results) # keep as separate datasets in list
  attr(results, "workflow") <- "nf-rnaseq"
  return(results)
}


#' Map sample to output from nf-sarek
#'
#' See https://nf-co.re/sarek. Similar to \code{\link{map_sample_output_rnaseq}} but for Sarek outputs.
#' Processed outputs have been seen to have variable organization, nested first by sample or by caller as
#' `VariantCalling/<SAMPLE>/<CALLER>` or as
#' `VariantCalling/<CALLER>/<SAMPLE>`.
#' The hierarchy/context needed for annotation can be obtained with a fileview and given the
#' necessary starting point `syn_out`, which is the id of the relevant output folder
#' (called `VariantCalling` or `variant_calling` usually).
#'
#' Note: Depending on when `map_sample_output_sarek` is run on the output directory,
#' there may be just `vcf` or both `vcf` and `maf` present. **`maf` are ignored since
#' technically it is not an output of sarek but from further processing with nf-vcf2maf.**
#'
#' @param syn_out Syn id of variant calling output folder.
#' @param fileview An existing fileview to use (usually the project's local fileview)
#' that scopes outputs and has "default" columns (`id`, `name`, `type`, `parentId`, `path`, ...). See details.
#' @param output Which output to select and annotate.
#' Defaults to checking the presence of possible prioritized outputs
#' ("CNVkit", "DeepVariant", "Strelka2", "Mutect2", and "FreeBayes"),
#' though typically only a subset makes sense and can be explicitly specified, which also speeds up results.
#' @import data.table
#' @return A `data.table` with cols `caller` `path` `output_name` `output_id` `sample` `workflow`.
#' An attribute `workflow=nf-sarek` will be set on the returned list,
#' and elements will have attribute `outputFrom` set, e.g. `outputFrom=CNVkit`.
#' @export
map_sample_output_sarek <- function(syn_out,
                                    fileview,
                                    output = c("CNVkit", "DeepVariant", "Strelka2", "Mutect2", "FreeBayes")) {

  output <- match.arg(output, several.ok = T)
  path <- get_path(syn_out)
  message("Path: ", path)

  results <- list()
  for(.output in output) {
    # Synapse `like` query is not case-sensitive
    path_spec <- switch(.output,
                        "CNVkit" = glue::glue("path like '{path}/%cnvkit%'"),
                        "DeepVariant" = glue::glue("path like '{path}/%deepvariant%vcf.gz%'"),
                        "Strelka2" = glue::glue("path like '{path}/%strelka%vcf.gz%'"),
                        "Mutect2" = glue::glue("path like '{path}/%mutect%%vcf.gz%'"),
                        "FreeBayes"= glue::glue("path like '{path}/%freebayes%vcf.gz%'"))
    query <- glue::glue("SELECT path, name as output_name, id as output_id from {fileview} where {path_spec} and type = 'file'")
    result <- as.data.table(.syn$tableQuery(query)$asDataFrame())

    message("Found ", nrow(result), " files for ", .output)

    if(nrow(result)) {
      result[, caller := .output]
      result[, workflow := .output]

      index_fun <- function(x) {
        caller_index <- grep("cnvkit|deepvariant|strelka|mutect|freebayes", x, ignore.case = TRUE)[1]
        if(!length(caller_index)) stop("Issue with figuring out sample output organization. Is there non-standard output?")
        file_index <- length(x)
        if((file_index - caller_index) == 1) {
          file_index - 2 # i.e. `VariantCalling/<SAMPLE>/<CALLER>`
        } else {
          file_index - 1 # i.e. `VariantCalling/<CALLER>/<SAMPLE>`
        }
      }
      result[, sample := path_extract(path, index_fun = index_fun)]
      results[[.output]] <- result
      setattr(results[[.output]], "outputFrom", .output)
    }
  }

  attr(results, "workflow") <- "nf-sarek"
  results
}


#' Map sample input-output
#'
#' Wrapper to map sample inputs and outputs depending on workflow type.
#'
#' @param input Data from `map_sample_input_ss`.
#' @param output Data from `map_sample_output_*`.
#' @param workflow Either "nf-rnaseq" or "nf-sarek".
#' @return A table with `sample` `level` `output_id` `output_name` `input_id`.
#' @export
map_sample_io <- function(input,
                          output,
                          workflow) {


  # Taking the first sample technically only matters for nf-sarek,
  # where sample can contain 2 samples (tumor vs normal from same indiv)
  if(workflow == "nf-sarek") output[, sample := sapply(sample, first)]

  # Check that sample ids in sample_outputs are in sample_inputs
  # Presumed OK for sample_inputs to contain samples *not* in outputs
  # (maybe no outputs if doesn't pass QC checks) but *not* OK vice versa
  if(!all(unique(output$sample) %in% unique(input$sample))) {
    stop("Issue found: samples present in outputs are not referenced in inputs.", call. = F)
  }
  sample_io <- merge(output, input, by = "sample", all.x = TRUE, all.y = FALSE)
  setattr(sample_io, "outputFrom", attr(output, "outputFrom"))
  return(sample_io)
}


#' Derive annotations for processed output data
#'
#' A processed or derived file can inherit annotations from the input file(s).
#' Currently, this generously facilitates inheritance of many properties except ones that
#' "obviously" shouldn't be inherited, such as "fileFormat" or "comments".
#' These rules are hard-coded and might need to be expanded as the data model changes.
#'
#' If multiple inputs given, this will inherit annotations from the FIRST input.
#'
#' @param sample_io Mapping of input and output files from a workflow.
#' @param template Which template to use when deriving annotations.
#' This controls which attributes are relevant for transfer/keep.
#' If not given, will use whatever is set for the attribute "template".
#' @keywords internal
derive_annotations <- function(sample_io,
                               template = NULL,
                               schema = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                               verbose = TRUE) {

  outputFrom <- attr(sample_io, "outputFrom")
  if(verbose) message(glue::glue("Deriving annotations for {nrow(sample_io)} files from {outputFrom}..."))

  if(is.null(template)) {
    template <- annotation_rule(outputFrom, "template")
  }
  props <- get_dependency_from_json_schema(id = template, schema = schema)

  datatype_attrs <-  c("Component", "fileFormat", "dataType", "dataSubtype")
  entity_attrs <- c("comments", "entityId", "progressReportNumber")
  props <- props[!props %in% c(datatype_attrs, entity_attrs)]

  from <- sapply(sample_io$input_id, `[`, 1)
  to <- sample_io$output_id
  annotations <- Map(function(f, t) copy_annotations(f, t, select = props), from, to) |>
    setNames(to) |>
    lapply(reticulate::py_to_r) |>
    rbindlist(fill = T, idcol = "entityId")

  metadata <- merge(annotations,
                    sample_io[, .(entityId = output_id, Filename = output_name, workflow)],
                    by = "entityId")

  setattr(metadata, "outputFrom", attr(sample_io, "outputFrom"))
  setattr(metadata, "workflow", attr(sample_io, "workflow"))
  setattr(metadata, "template", template)
  return(metadata)
}

#' Annotate processed data
#'
#' Annotate as some processed data type inferred from output.
#'
#' @param metadata Metadata.
#' @param ... Other parameters to pass to the called annotation function.
#' @export
annotate_processed <- function(metadata, ...) {

  outputFrom <- attr(metadata, "outputFrom")
  annotate_as <- annotation_rule(outputFrom, "annotate_as")
  params <- list(...)
  annotate_as(metadata, workflow_link = params$workflow_link)

}

#' Annotate processed aligned reads
#'
#' Given a manifest, annotate data as aligned reads data.
#' Returns a "partial" manifest, which can be adjusted as needed,
#' e.g. add additional comments or batch info.
#'
#' @param metadata Metadata table to build upon.
#' @param workflow_link Workflow link to most specific part of workflow generating these data.
#' @param genomic_reference For aligned reads, genomic reference meta should be present;
#' defaults to GRCh38.
#' @param verbose Give verbose reports for what's happening.
#' @export
annotate_aligned_reads <- function(metadata,
                                   workflow_link,
                                   genomic_reference = "GRCh38",
                                   verbose = TRUE) {

  outputFrom <- attr(metadata, "outputFrom")
  if(verbose) message("Running annotate_aligned_reads for ", outputFrom)

  format_as <- annotation_rule(outputFrom, "format_as")
  metadata[, fileFormat := format_as(Filename)]
  metadata[, dataType := "AlignedReads"]
  metadata[, dataSubtype := "processed"]
  metadata[, workflowLink := workflow_link]
  metadata[, genomicReference := genomic_reference]

  if(verbose) message(genomic_reference, " used for genomicReference")
  return(metadata)
}


#' Annotate quantified expression output
#'
#' Given a manifest, annotate data as level 3 processed expression data,
#' using defaults from star_salmon processing.
#' Returns a "partial" manifest, which can be adjusted as needed,
#' e.g. add additional comments or batch info.
#'
#' @inheritParams annotate_aligned_reads
#' @export
annotate_quantified_expression <- function(metadata,
                                           workflow_link,
                                           verbose = TRUE) {

  outputFrom <- attr(metadata, "outputFrom")
  if(verbose) message("Running annotate_quantified_expression for ", outputFrom)

  format_as <- annotation_rule(outputFrom, "format_as")
  expression_unit <- switch(outputFrom,
                            "STAR and Salmon" = "TPM",
                            "featureCounts" = "Counts")

  metadata[, fileFormat := format_as(Filename)]
  metadata[, dataType := "geneExpression"]
  metadata[, dataSubtype := "processed"]
  metadata[, expressionUnit := expression_unit]
  metadata[, workflowLink := workflow_link]

  if(verbose) message(expression_unit, " used for expressionUnit")
  return(metadata)
}


#' Annotate somatic or germline variants output
#'
#' Given a manifest, annotate data as variant data (`vcf` or `maf`).
#'
#' `maf` files use the same template but with different default values.
#' If in the future `maf`s require a significantly different template,
#' then this should be factored out into a separate annotation function.
#'
#' @inheritParams annotate_aligned_reads
#' @export
annotate_called_variants <- function(metadata,
                                     workflow_link,
                                     verbose = TRUE) {

    outputFrom <- attr(metadata, "outputFrom")
    if(verbose) message("Running annotate_called_variants for ",  outputFrom)

    # vcfs can be annotated type, workflow stops at Variant Calling bc we run a custom nf-vcf2maf
    data_type_assign <- function(name, format) {
      if(grepl("_vs_", name) && format == "vcf") {
        "SomaticVariants"
      } else if(!grepl("_vs_", name) && format == "vcf") {
        "GermlineVariants"
      } else if(grepl("_vs_", name) && format == "maf") {
        "AnnotatedSomaticVariants"
      } else if(!grepl("_vs_", name) && format == "maf") {
        "AnnotatedGermlineVariants"
      } else if(format %in% c("cns", "cnn", "cnr", "bed", "pdf", "png")) {
        "CopyNumberVariants"
      } else {
        stop("Not recognizable with data assignment rules.")
      }
    }

    format_as <- annotation_rule(outputFrom, "format_as")
    metadata[, fileFormat := format_as(Filename)]
    metadata[, dataType := data_type_assign(Filename, fileFormat), by = entityId]
    metadata[, dataSubtype := "processed"]
    metadata[, workflowLink := workflow_link]

    return(metadata)
}


#' Make annotations from workflow tool stats
#'
#' Extracts a subset of [samtools stats](http://www.htslib.org/doc/samtools-stats.html)
#' and [picard stats](https://broadinstitute.github.io/picard/picard-metric-definitions.html)
#' from workflow metafiles to surface as annotations. Note: picard stats only for WGS/WES/targeted sequencing.
#' Regarding the selection of stats, see the Genomic Data Commons (GDC) model for
#' [AlignedReads](https://docs.gdc.cancer.gov/Data_Dictionary/viewer/#?view=table-definition-view&id=aligned_reads)
#'
#' @param meta Data to which tool stats will be added as additional meta.
#' @param samtools_stats_file Path to file/syn id of file with samtools stats produced by the workflow.
#' @param picard_stats_file Path to file/syn id of file with picard stats produced by the workflow.
#' @export
annotate_with_tool_stats <- function(meta,
                                     samtools_stats_file = NULL,
                                     picard_stats_file = NULL) {
  if(is.null(samtools_stats_file)) {
    message("SAMtools stats not available, skipping these annotations...")
    sam_stats <- NULL
  } else {
    sam_stats <- dt_read(samtools_stats_file)
    sam_stats <- sam_stats[,
                           .(sample = Sample,
                             averageInsertSize = insert_size_average,
                             averageReadLength = average_length,
                             averageBaseQuality = average_quality,
                             pairsOnDifferentChr = pairs_on_different_chromosomes,
                             readsDuplicatedPercent = reads_duplicated_percent,
                             readsMappedPercent = reads_mapped_percent,
                             totalReads = raw_total_sequences)]
  }

  if(is.null(picard_stats_file)) {
    message("Picard stats not available, skipping these annotations...")
    picard_stats <- NULL
  } else { # TO DO
    picard_stats <- dt_read(picard_stats_file)
    picard_stats <- picard_stats[,
                                 .(sample = Sample,
                                   meanCoverage = MEAN_COVERAGE,
                                   proportionCoverage10x = PCT_10X,
                                   proportionCoverage30x = PCT_30X)]
  }
  result <- Reduce(function(x, y) merge(x, y, by = "sample"),
                   Filter(is.data.table, list(sam_stats, picard_stats)))
  if(!is.null(result) && !is.null(meta)) {
    result <- merge(meta[, .(sample, entityId = output_id, output_name)], result, by = "sample", all.x = TRUE)[grepl(".bam$", output_name)]
    for(col in c("sample", "output_name")) result[[col]] <- NULL
  }
  return(result)
}


#' Metadata for processed products
#'
#' Wrapper for all steps to get manifest for processed product
#'
#' @inheritParams map_sample_io
#' @param workflow_link Workflow link.
#' @export
#' @return List `manifest` with manifests for each processed dataset,
#' and `sample_io` with linked inputs and outputs (should be used for provenance annotation).
processed_meta <- function(input,
                           output,
                           workflow,
                           workflow_link) {

  workflow <- attr(output, "workflow")
  sample_io_list <- lapply(output, function(o) map_sample_io(input, o, workflow))
  sample_io <- rbindlist(sample_io_list)
  sample_io[, workflowLink := workflow_link]

  manifests <- lapply(sample_io_list, function(sample_io) {
    sample_io |>
      derive_annotations() |>
      annotate_processed(workflowLink = workflow_link)
  })
  return(
    list(manifests = manifests,
         sample_io = sample_io))
}
