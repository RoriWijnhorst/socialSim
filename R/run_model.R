#' Fit one of the available Stan models to simulated datasets
#'
#' @param sim Output from \code{simulate_data()}.
#' @param model Name of the Stan model to use (choose from the available options).
#' @param backend Either "cmdstanr" (default) to compile Stan code, or
#'        "precompiled" to run an existing executable in inst/stan/.
#' @param iter Number of iterations per chain.
#' @param seed Random seed for reproducibility.
#' @param cores Number of CPU cores (chains are run sequentially if backend = "precompiled").
#'
#' @return A list of fitted model summaries, one per dataset.
#' @export
run_model <- function(sim,
                      model = "Trait_only.stan",
                      backend = c("cmdstanr", "precompiled"),
                      iter = 1000,
                      seed = 1234,
                      cores = 1) {

  backend <- match.arg(backend)

  stopifnot(inherits(sim, "socialSim_data"))
  if (missing(cores) || !is.numeric(cores) || cores < 1)
    stop("Please specify a valid number of cores (e.g., cores = 4).")

  # Locate Stan model or precompiled executable
  model_path <- system.file("stan", model, package = "socialSim")
  exe_path <- sub("\\.stan$", ifelse(.Platform$OS.type == "windows", ".exe", ""), model_path)

  if (model_path == "" && exe_path == "")
    stop("Model not found in inst/stan/.")

  # ---------------------------------------------------------------------
  # BACKEND 1: CmdStanR (compile Stan model and run)
  # ---------------------------------------------------------------------
  if (backend == "cmdstanr") {

    if (!requireNamespace("cmdstanr", quietly = TRUE))
      stop("The 'cmdstanr' package is required for backend = 'cmdstanr'. Install it first.")

    if (is.null(cmdstanr::cmdstan_path()) || cmdstanr::cmdstan_path() == "")
      stop("CmdStan is not configured. Run cmdstanr::install_cmdstan() once before use.")

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

    # ---------------------------------------------------------------------
    # BACKEND 2: Precompiled executable (run binary directly)
    # ---------------------------------------------------------------------
  } else if (backend == "precompiled") {

    message("Running precompiled Stan executable for model: ", model)

    fit_one <- function(dat, id) {
      standata_file <- tempfile(fileext = ".json")
      output_dir <- tempfile()

      # Save data as JSON (CmdStan format)
      jsonlite::write_json(list(
        n_obs = dat$n_obs,
        n_ind = dat$n_ind,
        individual = dat$individual,
        opponent = dat$opponent,
        xj = dat$xj,
        z = dat$z
      ), path = standata_file, auto_unbox = TRUE)

      # Run executable using system2
      system2(
        command = exe_path,
        args = c("sample",
                 paste0("data file=", standata_file),
                 paste0("output file=", file.path(output_dir, paste0("output_", id, ".csv"))),
                 paste0("num_samples=", iter),
                 paste0("random seed=", seed + id)),
        stdout = TRUE,
        stderr = TRUE
      )

      # Read CmdStan CSV output
      out_file <- file.path(output_dir, paste0("output_", id, ".csv"))
      if (!file.exists(out_file))
        stop("Stan executable did not produce output. Check model name and permissions.")

      draws <- posterior::read_cmdstan_csv(out_file)
      list(summary = posterior::summarise_draws(draws), data_id = id)
    }
  }

  # ---------------------------------------------------------------------
  # Run all datasets sequentially (but support multiple cores)
  # ---------------------------------------------------------------------
  if (!requireNamespace("future.apply", quietly = TRUE))
    stop("Please install 'future.apply' for parallel execution.")

  future::plan(future::multisession, workers = cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  nsets <- length(sim$data)
  message("Running ", nsets, " datasets using backend: ", backend)

  results <- future.apply::future_lapply(seq_len(nsets),
                                         function(i) fit_one(sim$data[[i]], i),
                                         future.seed = TRUE)

  structure(results, class = "socialSim_results")
}
