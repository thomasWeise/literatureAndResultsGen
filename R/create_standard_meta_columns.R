#' @title Create the Standard Meta-Data Column Set
#' @description Create the set of standard meta-data columns
#' @param are.objective.values.ints are objective values integers?
#' @param objective.value.lower.bound a hard lower bound for objective values
#' @param objective.value.upper.bound a hard upper bound for objective values
#' @return the set of standard meta-data columns
#' @include names.R
#' @include create_stat_columns.R
#' @export create.standard.meta.columns
#' @include base_conditions.R
#' @include create_column.R
create.standard.meta.columns <- function(are.objective.values.ints=TRUE,
                                         objective.value.lower.bound=0L,
                                         objective.value.upper.bound=NA_integer_) {
  old.options <- options(warn=2);

  algo.id <- create.column(.col.algo.id,
                           "the ID or short name of the algorithm used to solve the problem instance",
                           "character");
  algo.desc <- create.column(.col.algo.desc,
                           "a short description of the algorithm",
                           "character");
  inst.id <- create.column(.col.inst.id,
                           "the ID of the problem instance solved",
                           "character");
  ref.id <- create.column(.col.ref.id,
                          "the id of the bibliography entry identifying the publication where the result was taken from",
                          "character");
  ref.year <- create.column(.col.ref.year,
                            "the year when the results were published",
                            "integer");

  if(are.objective.values.ints) {
    lbt <- "integer";
  } else {
    lbt <- "double";
  }
  inst.opt.bound.lower <- create.column(.col.inst.opt.bound.lower,
                           "lower bound of the optimal objective value",
                           lbt);

  system.cpu.name <- create.column(.col.system.cpu.name,
                                   "the name of the CPU of the system on which the experiment was run",
                                   "character");
  system.cpu.n <- create.column(.col.system.cpu.n,
                                "the number of CPUs used",
                                "integer");
  system.cpu.cores <- create.column(.col.system.cpu.cores,
                                    "the number of cores of per CPU",
                                    "integer");
  system.cpu.threads <- create.column(.col.system.cpu.threads,
                                      "the number of threads of per CPU",
                                      "integer");
  system.cpu.mhz <- create.column(.col.system.cpu.mhz,
                                  "the clock speed of the CPU in MHz",
                                  "integer");

  system.runs.are.parallel <- create.column(.col.system.runs.are.parallel,
                                            "are the single runs making use of parallelization, i.e., do they use multiple threads?",
                                            "logical");

  system.memory.mb <- create.column(.col.system.memory.mb,
                                    "the memory size of the system in MB",
                                    "integer");
  system.os.name  <- create.column(.col.system.os.name,
                                   "the name of the CPU of the system on which the experiment was run",
                                   "character");
  system.vm.name  <- create.column(.col.system.vm.name,
                                   "the name of the virtual machine used to execute the experiment, if any",
                                   "character");
  system.compiler.name  <- create.column(.col.system.compiler.name,
                                         "the name of the compiler used to compile the programs for the experiment",
                                         "character");
  system.programming.language.name  <- create.column(.col.system.programming.language.name,
                                         "the name of the programming language in which the program was written",
                                         "character");
  system.external.tools.list  <- create.column(.col.system.external.tools.list,
                                              "the name of the external tools, such as deterministic solvers or CPLEX, whatever was used in the experiments, separated by ';'",
                                              "character");
  notes <- create.column(.col.notes,
                         "additional notes",
                         "character");

  result <- create.columns(list(algo.id=algo.id,
                                algo.desc=algo.desc,
                                ref.id=ref.id,
                                ref.year=ref.year,
                                inst.id=inst.id,
                                inst.opt.bound.lower=inst.opt.bound.lower,
                                system.cpu.name=system.cpu.name,
                                system.cpu.n=system.cpu.n,
                                system.cpu.cores=system.cpu.cores,
                                system.cpu.threads=system.cpu.threads,
                                system.cpu.mhz=system.cpu.mhz,
                                system.runs.are.parallel=system.runs.are.parallel,
                                system.memory.mb=system.memory.mb,
                                system.os.name=system.os.name,
                                system.vm.name=system.vm.name,
                                system.compiler.name=system.compiler.name,
                                system.programming.language.name=system.programming.language.name,
                                system.external.tools.list=system.external.tools.list,
                                notes=notes),
                           conditions = c(
                             paste0("all(!is.na(x$", algo.id$title, "))"),
                             paste0("is.character(x$", algo.id$title, ")"),
                             paste0("all(nchar(x$", algo.id$title, ") > 0L)"),

                             paste0("all(!is.na(x$", algo.desc$title, "))"),
                             paste0("is.character(x$", algo.desc$title, ")"),
                             paste0("all(nchar(x$", algo.desc$title, ") > 0L)"),

                             paste0("all(!is.na(x$", inst.id$title, "))"),
                             paste0("is.character(x$", inst.id$title, ")"),
                             paste0("all(nchar(x$", inst.id$title, ") > 0L)"),

                             paste0("all(!is.na(x$", ref.id$title, "))"),
                             paste0("is.character(x$", ref.id$title, ")"),
                             paste0("all(nchar(x$", ref.id$title, ") > 0L)"),

                             paste0("all(!is.na(x$", inst.opt.bound.lower$title, "))"),
                             paste0("all(is.finite(x$", inst.opt.bound.lower$title, "))"),

                             paste0("all(is.character(x$", system.cpu.name$title, "))"),
                             paste0("all(is.na(x$", system.cpu.name$title, ") | (nchar(x$", system.cpu.name$title, ") > 0L))"),

                             paste0("all(is.integer(x$", system.cpu.n$title, "))"),
                             paste0("all(is.na(x$", system.cpu.n$title, ") | (is.finite(x$", system.cpu.n$title, ") & (x$", system.cpu.n$title, " > 0L)))"),

                             paste0("all(is.integer(x$", system.cpu.cores$title, "))"),
                             paste0("all(is.na(x$", system.cpu.cores$title, ") | (is.finite(x$", system.cpu.cores$title, ") & (x$", system.cpu.cores$title, " > 0L)))"),

                             paste0("all(is.integer(x$", system.cpu.threads$title, "))"),
                             paste0("all(is.na(x$", system.cpu.threads$title, ") | (is.finite(x$", system.cpu.threads$title, ") & (x$", system.cpu.threads$title, " > 0L)))"),

                             paste0("all(is.integer(x$", system.cpu.mhz$title, "))"),
                             paste0("all(is.na(x$", system.cpu.mhz$title, ") | (is.finite(x$", system.cpu.mhz$title, ") & (x$", system.cpu.mhz$title, " > 0L)))"),

                             paste0("all(is.integer(x$", system.memory.mb$title, "))"),
                             paste0("all(is.na(x$", system.memory.mb$title, ") | (is.finite(x$", system.memory.mb$title, ") & (x$", system.memory.mb$title, " > 0L)))"),

                             paste0("all(is.character(x$", system.os.name$title, "))"),
                             paste0("all(is.na(x$", system.os.name$title, ") | (nchar(x$", system.os.name$title, ") > 0L))"),

                             paste0("all(is.character(x$", system.vm.name$title, "))"),
                             paste0("all(is.na(x$", system.vm.name$title, ") | (nchar(x$", system.vm.name$title, ") > 0L))"),

                             paste0("all(is.character(x$", system.compiler.name$title, "))"),
                             paste0("all(is.na(x$", system.compiler.name$title, ") | (nchar(x$", system.compiler.name$title, ") > 0L))"),

                             paste0("all(is.character(x$", system.programming.language.name$title, "))"),
                             paste0("all(is.na(x$", system.programming.language.name$title, ") | (nchar(x$", system.programming.language.name$title, ") > 0L))"),

                             paste0("all(is.character(x$", system.external.tools.list$title, "))"),
                             paste0("all(is.na(x$", system.external.tools.list$title, ") | (nchar(x$", system.external.tools.list$title, ") > 0L))"),

                             paste0("all(is.logical(x$", system.runs.are.parallel$title, "))"),

                             paste0("all(is.character(x$", notes$title, "))"),
                             paste0("all(is.na(x$", notes$title, ") | (nchar(x$", notes$title, ") > 0L))")

                           ));
  result <- force(result);
  options(old.options);
  return(result);
}
