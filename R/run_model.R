#' Fit one of the available Stan models to simulated datasets
#'
#' @param sim Output from \code{simulate_data()}.
#' @param model Name of the Stan model to use (choose from the 5 available options).
#' @param iter Number of iterations per chain (default = 1000).
#' @param seed Random seed for reproducibility.
#' @param cores Number of CPU cores to use (required).
#'
#' @return A list of fitted model summaries, one per dataset.
#' @export
run_model <- function(sim,
                      model = NULL,
                      iter = 1000,
                      seed = 1234,
                      cores) {

  # -------------------------------------------------------------------------
  # 1. Available models
  available_models <- c(
    "I&R.stan",
    "VP.stan",
    "Trait.stan",
    "Trait_only.stan",
    "Trait_RS.stan",
    "Trait_EIV.stan"
  )

  # 2. Handle missing or invalid input
  if (is.null(model)) {
    message("Please choose one of the following available models:\n",
            paste0("  - ", available_models, collapse = "\n"))
    stop("No model selected. Specify a model, e.g. model = 'Trait.stan'.")
  }

  if (!model %in% available_models) {
    stop("Invalid model name. Available options are:\n",
         paste0("  - ", available_models, collapse = "\n"))
  }

  # -------------------------------------------------------------------------
  # 3. Interactive parameter compatibility checks
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
  # 4. Locate Stan file inside your package
  model_path <- system.file("stan", model, package = "socialSim")
  if (model_path == "") {
    stop("Stan model file not found in inst/stan/. Check your package structure.")
  }

  # -------------------------------------------------------------------------
  # 5. Input checks and setup
  stopifnot(inherits(sim, "socialSim_data"))
  if (missing(cores) || !is.numeric(cores) || cores < 1)
    stop("Please specify a valid number of cores, e.g. cores = 4.")

  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    stop("Packages 'future' and 'future.apply' must be installed for parallel execution.")
  }

  mod <- cmdstanr::cmdstan_model(model_path)

  # -------------------------------------------------------------------------
  # 6. Define the per-dataset fitting function
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

    list(
      summary = fit$summary(),
      params = sim$params,
      design = sim$design,
      data_id = id
    )
  }

  # -------------------------------------------------------------------------
  # 7. Parallel execution
  future::plan(future::multisession, workers = cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  nsets <- length(sim$data)
  message("Running ", nsets, " datasets on ", cores, " cores using model: ", model)

  results <- future.apply::future_lapply(
    seq_len(nsets),
    function(i) fit_one(sim$data[[i]], i),
    future.seed = TRUE
  )

  message("All datasets finished for model: ", model)

  structure(results, class = "socialSim_results")
}
