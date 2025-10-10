#' Fit one of the available Stan models to simulated datasets
#'
#' @param sim Output from \code{simulate_data()}.
#' @param model Name of the Stan model to use (choose from available options).
#' @param iter Number of iterations per chain (default = 1000).
#' @param seed Random seed for reproducibility.
#' @param cores Number of CPU cores (used if cmdstanr is available).
#'
#' @return A list of fitted model summaries, one per dataset.
#' @export
run_model <- function(sim,
                      model = NULL,
                      iter = 1000,
                      seed = 1234,
                      cores = 1) {

  stopifnot(inherits(sim, "socialSim_data"))

  if (missing(cores) || !is.numeric(cores) || cores < 1)
    stop("Please specify a valid number of cores (e.g., cores = 4).")

  # -------------------------------------------------------------------------
  # 1. Define available models
  # -------------------------------------------------------------------------
  available_models <- c(
    "I&R.stan",
    "VP.stan",
    "Trait.stan",
    "Trait_only.stan",
    "Trait_RS.stan",
    "Trait_EIV.stan"
  )

  # -------------------------------------------------------------------------
  # 2. Handle missing or invalid input
  # -------------------------------------------------------------------------
  if (is.null(model)) {
    message("Please choose one of the following available models:\n",
            paste0("  - ", available_models, collapse = "\n"))
    stop("No model selected. Specify a model, e.g. model = 'Trait.stan'.")
  }

  if (!model %in% available_models) {
    stop("Invalid model name. Available options are:\n",
         paste0("  - ", available_models, collapse = "\n"))
  }

  # Locate Stan model and executable
  model_path <- system.file("stan", model, package = "socialSim")
  exe_path <- sub("\\.stan$", ifelse(.Platform$OS.type == "windows", ".exe", ""), model_path)
  if (model_path == "" && exe_path == "")
    stop("Model not found in inst/stan/. Please check model name.")

  # -------------------------------------------------------------------------
  # 3. Interactive parameter compatibility checks
  # -------------------------------------------------------------------------
  params <- sim$params

  # Case 1: measurement error missing
  if (model %in% c("I&R.stan", "Trait_EIV.stan") && params$Vxe == 0) {
    message(
      "\nThe selected model (", model, ") estimates measurement error or variation in the partner trait (Vxe).",
      "\nHowever, Vxe = 0 in your simulated data."
    )
    ans <- utils::menu(c("Yes", "No"),
                       title = "Would you like to continue anyway?")
    if (ans == 2) stop("Model fitting cancelled by user.")
  }

  # Case 2: responsiveness variance missing
  if (model %in% c("I&R.stan", "Trait_RS.stan") && params$Vpsi == 0) {
    message(
      "\nThe selected model (", model, ") estimates individual variation in responsiveness (Vpsi).",
      "\nHowever, Vpsi = 0 in your simulated data."
    )
    ans <- utils::menu(c("Yes", "No"),
                       title = "Would you like to continue anyway?")
    if (ans == 2) stop("Model fitting cancelled by user.")
  }

  # -------------------------------------------------------------------------
  # 4. Automatic backend detection
  # -------------------------------------------------------------------------
  use_cmdstanr <- requireNamespace("cmdstanr", quietly = TRUE)

  if (use_cmdstanr) {
    if (is.null(cmdstanr::cmdstan_path()) || cmdstanr::cmdstan_path() == "") {
      message("CmdStanR detected, but CmdStan not yet installed.")
      message("You can install it by running: cmdstanr::install_cmdstan()")
      use_cmdstanr <- FALSE
    }
  }

  if (!use_cmdstanr) {
    message("------------------------------------------------------------")
    message("CmdStanR not detected or not configured.")
    message("It is recommended to install CmdStanR for faster, parallel Bayesian sampling.")
    message("To install, run: cmdstanr::install_cmdstan()")
    message("")
    ans <- utils::menu(c("Yes", "No"),
                       title = "Continue with slower, sequential execution using precompiled models?")
    if (ans == 2)
      stop("Aborted. Please install CmdStanR first.")
    backend <- "precompiled"
    message("Running with precompiled backend.")
  } else {
    backend <- "cmdstanr"
    message("Running with cmdstanr backend.")
  }

  # -------------------------------------------------------------------------
  # 5. Define fitting functions
  # -------------------------------------------------------------------------
  if (backend == "cmdstanr") {
    mod <- cmdstanr::cmdstan_model(model_path)

    fit_one <- function(dat, id) {
      standata <- list(
        n_obs = dat$n_obs,
        n_ind = dat$n_ind,
        individual = dat$individual,
        opponent = dat$opponent,
        xj = dat$xj,
        z = dat$z
      )

      fit <- mod$sample(
        data = standata,
        iter_sampling = iter,
        iter_warmup = iter / 2,
        chains = 1,
        parallel_chains = 1,
        seed = seed + id,
        refresh = 0
      )

      list(summary = fit$summary(), data_id = id)
    }

  } else {
    fit_one <- function(dat, id) {
      standata_file <- tempfile(fileext = ".json")
      output_dir <- tempfile()

      jsonlite::write_json(list(
        n_obs = dat$n_obs,
        n_ind = dat$n_ind,
        individual = dat$individual,
        opponent = dat$opponent,
        xj = dat$xj,
        z = dat$z
      ), path = standata_file, auto_unbox = TRUE)

      system2(
        command = exe_path,
        args = c("sample",
                 paste0("data file=", standata_file),
                 paste0("output file=", file.path(output_dir, paste0("output_", id, ".csv"))),
                 paste0("num_samples=", iter),
                 paste0("random seed=", seed + id)),
        stdout = TRUE, stderr = TRUE
      )

      out_file <- file.path(output_dir, paste0("output_", id, ".csv"))
      if (!file.exists(out_file))
        stop("Stan executable did not produce output. Check model name and permissions.")

      draws <- posterior::read_cmdstan_csv(out_file)
      list(summary = posterior::summarise_draws(draws), data_id = id)
    }
  }

  # -------------------------------------------------------------------------
  # 6. Run datasets
  # -------------------------------------------------------------------------
  if (!requireNamespace("future.apply", quietly = TRUE))
    stop("Please install 'future.apply' for parallel execution.")

  future::plan(future::multisession, workers = cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  nsets <- length(sim$data)
  message("Running ", nsets, " datasets on ", cores, " cores (backend: ", backend, ")...")

  results <- future.apply::future_lapply(seq_len(nsets),
                                         function(i) fit_one(sim$data[[i]], i),
                                         future.seed = TRUE)

  message("All datasets finished.")
  structure(results, class = "socialSim_results")
}
