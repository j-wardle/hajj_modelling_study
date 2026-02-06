# Wrapper functions for using dynamic movement inputs ----

## Wrapper function to use run_patch_model with dynamic movement matrix ----

## Output is a list of lists
## Level one: one list element per simulation run
## Level two: one list element per timestep model is run before
## Level three: two elements. 1 is the model state at that point in time, 2 is movement matrix

run_patch_model_vary_movement <- function(movement, dt, sims, starting_state) {

  model_output <- vector(mode = "list", length = sims)

  for (sim in seq_len(sims)) {

    model_output[[sim]] <- vector(mode = "list", length = length(movement))

    model_output[[sim]][[1]] <- run_patch_model(
      state_in = state(
        s_patches = starting_state$s_patches,
        e_patches = starting_state$e_patches,
        i_patches = starting_state$i_patches,
        r_patches = starting_state$r_patches,
        birth_rates = starting_state$birth_rates,
        death_rates = starting_state$death_rates,
        transmission_rates = starting_state$transmission_rates,
        infection_rates = starting_state$infection_rates,
        recovery_rates = starting_state$recovery_rates, # day^-1
        movement_rate = movement[[1]]
      ),
      dt = dt[1],
      nsim = 1
    )

    final_state <- as.data.frame(model_output[[sim]][[1]][[1]][[dt[1]]])

    for (i in 2:length(movement)) {

      model_output[[sim]][[i]] <- run_patch_model(
        state_in = state(
          s_patches = final_state$value[final_state$variable == "susceptible"],
          e_patches = final_state$value[final_state$variable == "exposed"],
          i_patches = final_state$value[final_state$variable == "infected"],
          r_patches = final_state$value[final_state$variable == "recovered"],
          birth_rates = starting_state$birth_rates,
          death_rates = starting_state$death_rates,
          transmission_rates = starting_state$transmission_rates,
          infection_rates = starting_state$infection_rates,
          recovery_rates = starting_state$recovery_rates, # day^-1
          movement_rate = movement[[i]]
        ),
        dt = dt[i]+1, # add 1 because the first state is from the last day of previous time period
        nsim = 1
      )

      final_state <- as.data.frame(model_output[[sim]][[i]][[1]][[dt[i]+1]])

      # remove the first state from month 2 as it is repeat of last state in month 1
      model_output[[sim]][[i]] <- list(model_output[[sim]][[i]][[1]][2:(dt[i]+1)])

    }

    # New way to combine outputs (less code and works for any number of movement phases)
    # See screening_vary_movement function if previous code needed
    output <- c()
    for (i in 1:length(movement)) {
      output <- c(output, model_output[[sim]][[i]][[1]])
    }
    model_output[[sim]] <- output


  }

  model_output

}

## Wrapper function for using run_patch_model_symptoms with dynamic movement matrix ----

run_patch_model_symptoms_vary_movement <- function(movement, dt, sims, starting_state) {

  model_output <- vector(mode = "list", length = sims)

  for (sim in seq_len(sims)) {

    model_output[[sim]] <- vector(mode = "list", length = length(movement))

    model_output[[sim]][[1]] <- run_patch_model_symptoms(
      state_in = multipatchr:::state_symptoms(
        s_patches = starting_state$s_patches,
        e_patches = starting_state$e_patches,
        i_a_patches = starting_state$i_a_patches,
        i_p_patches = starting_state$i_p_patches,
        i_s_patches = starting_state$i_s_patches,
        r_patches = starting_state$r_patches,
        birth_rates = starting_state$birth_rates,
        death_rates = starting_state$death_rates,
        transmission_rates = starting_state$transmission_rates,
        asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
        presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
        prop_symptomatic = starting_state$prop_symptomatic,
        infection_rates = starting_state$infection_rates,
        symptom_rates = starting_state$symptom_rates,
        recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
        recovery_rates_sym = starting_state$recovery_rates_sym,
        movement_rate = movement_phases[[1]]
      ),
      dt = dt[1],
      nsim = 1
    )

    final_state <- as.data.frame(model_output[[sim]][[1]][[1]][[dt[1]]])

    for (i in 2:length(movement)) {

      model_output[[sim]][[i]] <- run_patch_model_symptoms(
        state_in = multipatchr:::state_symptoms(
          s_patches = final_state$value[final_state$variable == "susceptible"],
          e_patches = final_state$value[final_state$variable == "exposed"],
          i_a_patches = final_state$value[final_state$variable == "infected_asymptomatic"],
          i_p_patches = final_state$value[final_state$variable == "infected_presymptomatic"],
          i_s_patches = final_state$value[final_state$variable == "infected_symptomatic"],
          r_patches = final_state$value[final_state$variable == "recovered"],
          birth_rates = starting_state$birth_rates,
          death_rates = starting_state$death_rates,
          transmission_rates = starting_state$transmission_rates,
          asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
          presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
          prop_symptomatic = starting_state$prop_symptomatic,
          infection_rates = starting_state$infection_rates,
          symptom_rates = starting_state$symptom_rates,
          recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
          recovery_rates_sym = starting_state$recovery_rates_sym,
          movement_rate = movement_phases[[i]]
        ),
        dt = dt[i]+1, # add 1 because the first state is from the last day of previous time period
        nsim = 1
      )

      final_state <- as.data.frame(model_output[[sim]][[i]][[1]][[dt[i]+1]])

      # remove the first state from month 2 as it is repeat of last state in month 1
      model_output[[sim]][[i]] <- list(model_output[[sim]][[i]][[1]][2:(dt[i]+1)])

    }

    # New way to combine outputs (less code and works for any number of movement phases)
    # See screening_vary_movement function if previous code needed
    output <- c()
    for (i in 1:length(movement)) {
      output <- c(output, model_output[[sim]][[i]][[1]])
    }
    model_output[[sim]] <- output

  }

  model_output

}

## Wrapper function for using run_patch_model_screening with dynamic movement matrix ----

run_patch_model_screening_vary_movement <- function(movement, dt, sims, starting_state) {

  model_output <- vector(mode = "list", length = sims)

  for (sim in seq_len(sims)) {

    model_output[[sim]] <- vector(mode = "list", length = length(movement))

    model_output[[sim]][[1]] <- run_patch_model_screening_incomingphase(
      state_in = multipatchr:::state_symptoms_testing(
        s_patches = starting_state$s_patches,
        e_patches = starting_state$e_patches,
        i_a_patches = starting_state$i_a_patches,
        i_p_patches = starting_state$i_p_patches,
        i_s_patches = starting_state$i_s_patches,
        r_patches = starting_state$r_patches,
        e_diag_patches = starting_state$e_diag_patches,
        i_a_diag_patches = starting_state$i_a_diag_patches,
        i_p_diag_patches = starting_state$i_p_diag_patches,
        i_s_diag_patches = starting_state$i_s_diag_patches,
        r_diag_patches = starting_state$r_diag_patches,
        s_false_patches = starting_state$s_false_patches,
        r_false_patches = starting_state$r_false_patches,
        birth_rates = starting_state$birth_rates,
        death_rates = starting_state$death_rates,
        transmission_rates = starting_state$transmission_rates,
        asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
        presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
        prop_symptomatic = starting_state$prop_symptomatic,
        infection_rates = starting_state$infection_rates,
        symptom_rates = starting_state$symptom_rates,
        recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
        recovery_rates_sym = starting_state$recovery_rates_sym,
        testing_rates = starting_state$testing_rate,
        test_sensitivity = starting_state$test_sensitivity,
        test_specificity = starting_state$test_specificity,
        isolation_period = starting_state$isolation_period,
        movement_rate = movement_phases[[1]]
      ),
      dt = dt[1],
      nsim = 1
    )

    final_state <- as.data.frame(model_output[[sim]][[1]][[1]][[dt[1]]])

    for (i in 2:length(movement)) {

      model_output[[sim]][[i]] <- run_patch_model_screening_otherphases(
        state_in = multipatchr:::state_symptoms_testing(
          s_patches = final_state$value[final_state$variable == "susceptible"],
          e_patches = final_state$value[final_state$variable == "exposed"],
          i_a_patches = final_state$value[final_state$variable == "infected_asymptomatic"],
          i_p_patches = final_state$value[final_state$variable == "infected_presymptomatic"],
          i_s_patches = final_state$value[final_state$variable == "infected_symptomatic"],
          r_patches = final_state$value[final_state$variable == "recovered"],
          e_diag_patches = final_state$value[final_state$variable == "exposed_diagnosed"],
          i_a_diag_patches = final_state$value[final_state$variable == "infected_asymptomatic_diagnosed"],
          i_p_diag_patches = final_state$value[final_state$variable == "infected_presymptomatic_diagnosed"],
          i_s_diag_patches = final_state$value[final_state$variable == "infected_symptomatic_diagnosed"],
          r_diag_patches = final_state$value[final_state$variable == "recovered_diagnosed"],
          s_false_patches = final_state$value[final_state$variable == "susceptible_false_positive"],
          r_false_patches = final_state$value[final_state$variable == "recovered_false_positive"],
          birth_rates = starting_state$birth_rates,
          death_rates = starting_state$death_rates,
          transmission_rates = starting_state$transmission_rates,
          asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
          presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
          prop_symptomatic = starting_state$prop_symptomatic,
          infection_rates = starting_state$infection_rates,
          symptom_rates = starting_state$symptom_rates,
          recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
          recovery_rates_sym = starting_state$recovery_rates_sym,
          testing_rates = starting_state$testing_rate,
          test_sensitivity = starting_state$test_sensitivity,
          test_specificity = starting_state$test_specificity,
          isolation_period = starting_state$isolation_period,
          movement_rate = movement_phases[[i]]
        ),
        dt = dt[i]+1, # add 1 because the first state is from the last day of previous time period
        nsim = 1
      )

      final_state <- as.data.frame(model_output[[sim]][[i]][[1]][[dt[i]+1]])

      # remove the first state from month 2 as it is repeat of last state in month 1
      model_output[[sim]][[i]] <- list(model_output[[sim]][[i]][[1]][2:(dt[i]+1)])

    }

    # New way to combine outputs (less code and works for any number of movement phases)
    output <- c()
    for (i in 1:length(movement)) {
      output <- c(output, model_output[[sim]][[i]][[1]])
    }
    model_output[[sim]] <- output

  }

  model_output

}

# Alternative version that uses run_patch_model_screening_incomingphase of model run
# function throughout. This allows us to have additional testing if we want,
# eg testing pilgrims as they return to their home countries.
run_patch_model_screening_vary_movement2 <- function(movement, dt, sims, starting_state,
                                                     additional_testing_rates = c(0,0)) {

  model_output <- vector(mode = "list", length = sims)

  for (sim in seq_len(sims)) {

    model_output[[sim]] <- vector(mode = "list", length = length(movement))

    model_output[[sim]][[1]] <- run_patch_model_screening_incomingphase(
      state_in = multipatchr:::state_symptoms_testing(
        s_patches = starting_state$s_patches,
        e_patches = starting_state$e_patches,
        i_a_patches = starting_state$i_a_patches,
        i_p_patches = starting_state$i_p_patches,
        i_s_patches = starting_state$i_s_patches,
        r_patches = starting_state$r_patches,
        diag_patches = starting_state$diag_patches,
        # e_diag_patches = starting_state$e_diag_patches,
        # i_a_diag_patches = starting_state$i_a_diag_patches,
        # i_p_diag_patches = starting_state$i_p_diag_patches,
        # i_s_diag_patches = starting_state$i_s_diag_patches,
        # r_diag_patches = starting_state$r_diag_patches,
        s_false_patches = starting_state$s_false_patches,
        r_false_patches = starting_state$r_false_patches,
        birth_rates = starting_state$birth_rates,
        death_rates = starting_state$death_rates,
        transmission_rates = starting_state$transmission_rates,
        transmission_rates_pilgrims = starting_state$transmission_rates_pilgrims,
        transmission_rates_pilgrims_and_atrisk = starting_state$transmission_rates_pilgrims_and_atrisk,
        asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
        presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
        prop_symptomatic = starting_state$prop_symptomatic,
        infection_rates = starting_state$infection_rates,
        symptom_rates = starting_state$symptom_rates,
        recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
        recovery_rates_sym = starting_state$recovery_rates_sym,
        testing_rates = starting_state$testing_rate,
        test_sensitivity = starting_state$test_sensitivity,
        test_specificity = starting_state$test_specificity,
        isolation_period = starting_state$isolation_period,
        movement_rate = movement_phases[[1]]
      ),
      dt = dt[1],
      nsim = 1
    )

    # Get the last 10 days so that we can use values of new_false_positives to determine the numbers leaving isolation later on
    final_ten_days <- model_output[[sim]][[1]][[1]][(dt[1]-9):dt[1]]

    final_state <- as.data.frame(model_output[[sim]][[1]][[1]][[dt[1]]])

    for (i in 2:length(movement)) {

      # If Hajj phase is short (i.e. 5 days), we will still have some people isolating at the
      # end of that period. Therefore, we need to rollover the unused values from final_ten_days.
      # We can do this as follows.

      threshold <- dt[2]

      if (i==3 & dt[2] < 10) {
        # browser()
        final_ten_days <- model_output[[sim]][[1]][[1]][(dt[1]-dt[2]+1):dt[1]]

        threshold <- 10 - dt[2]

      }

      model_output[[sim]][[i]] <- run_patch_model_screening_otherphases(
        state_in = multipatchr:::state_symptoms_testing(
          s_patches = final_state$value[final_state$variable == "susceptible"],
          e_patches = final_state$value[final_state$variable == "exposed"],
          i_a_patches = final_state$value[final_state$variable == "infected_asymptomatic"],
          i_p_patches = final_state$value[final_state$variable == "infected_presymptomatic"],
          i_s_patches = final_state$value[final_state$variable == "infected_symptomatic"],
          r_patches = final_state$value[final_state$variable == "recovered"],
          diag_patches = final_state$value[final_state$variable == "all_diagnosed"],
          # e_diag_patches = final_state$value[final_state$variable == "exposed_diagnosed"],
          # i_a_diag_patches = final_state$value[final_state$variable == "infected_asymptomatic_diagnosed"],
          # i_p_diag_patches = final_state$value[final_state$variable == "infected_presymptomatic_diagnosed"],
          # i_s_diag_patches = final_state$value[final_state$variable == "infected_symptomatic_diagnosed"],
          # r_diag_patches = final_state$value[final_state$variable == "recovered_diagnosed"],
          s_false_patches = final_state$value[final_state$variable == "susceptible_false_positive"],
          r_false_patches = final_state$value[final_state$variable == "recovered_false_positive"],
          birth_rates = starting_state$birth_rates,
          death_rates = starting_state$death_rates,
          transmission_rates = starting_state$transmission_rates,
          transmission_rates_pilgrims = starting_state$transmission_rates_pilgrims,
          transmission_rates_pilgrims_and_atrisk = starting_state$transmission_rates_pilgrims_and_atrisk,
          asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
          presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
          prop_symptomatic = starting_state$prop_symptomatic,
          infection_rates = starting_state$infection_rates,
          symptom_rates = starting_state$symptom_rates,
          recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
          recovery_rates_sym = starting_state$recovery_rates_sym,
          testing_rates = additional_testing_rates[i-1],
          test_sensitivity = starting_state$test_sensitivity,
          test_specificity = starting_state$test_specificity,
          isolation_period = starting_state$isolation_period,
          movement_rate = movement_phases[[i]]
        ),
        old_states = final_ten_days,
        old_states_threshold = threshold,
        dt = dt[i]+1, # add 1 because the first state is from the last day of previous time period
        nsim = 1
      )

      final_state <- as.data.frame(model_output[[sim]][[i]][[1]][[dt[i]+1]])

      # remove the first state from month 2 as it is repeat of last state in month 1
      model_output[[sim]][[i]] <- list(model_output[[sim]][[i]][[1]][2:(dt[i]+1)])

    }

    # New way to combine outputs (less code and works for any number of movement phases)
    output <- c()
    for (i in 1:length(movement)) {
      output <- c(output, model_output[[sim]][[i]][[1]])
    }

    # Remove the parameters from model output to reduce size
    output_to_keep <- c(
      "susceptible", "susceptible_false_positive",
      "exposed",
      # "exposed_diagnosed",
      "infected_asymptomatic",
      # "infected_asymptomatic_diagnosed",
      "infected_presymptomatic",
      # "infected_presymptomatic_diagnosed",
      "infected_symptomatic",
      # "infected_symptomatic_diagnosed",
      "recovered",
      # "recovered_diagnosed",
      "recovered_false_positive",
      "all_diagnosed",
      "imported_susceptible", "imported_exposed",
      "imported_infected_asymptomatic", "imported_infected_presymptomatic",
      "imported_infected_symptomatic", "imported_recovered",
      "new_exposed_diagnosed", "new_false_neg_exposed",
      "new_infected_asymptomatic_diagnosed", "new_false_neg_infected_asymptomatic",
      "new_infected_presymptomatic_diagnosed", "new_false_neg_infected_presymptomatic",
      "new_infected_symptomatic_diagnosed", "new_false_neg_infected_symptomatic",
      "new_susceptible_false_positive", "new_recovered_false_positive",
      "new_true_neg_susceptible", "new_true_neg_recovered",
      "released_s_false", "released_r_false",
      "released_exposed", "released_infected_asymptomatic",
      "released_infected_presymptomatic", "released_infected_symptomatic",
      "released_recovered"
    )

    output <- map(output, function(time) {

      map(time[["patches"]], function(patch) {

        patch[names(patch) %in% output_to_keep]

      })
    })

    model_output[[sim]] <- output
    # model_output[[sim]] <- format_model_output(output) %>% 
    #   select(-sim)

  }

  # model_output
  model_output <- format_model_output(model_output) %>%
    select(-sim)

}

# New wrapper for when we do symptomatic testing ----

run_patch_model_symptom_screening_vary_movement <- function(movement, dt, sims, starting_state,
                                                            additional_testing_rates = c(0,0)) {
  
  model_output <- vector(mode = "list", length = sims)
  
  for (sim in seq_len(sims)) {
    
    model_output[[sim]] <- vector(mode = "list", length = length(movement))
    
    model_output[[sim]][[1]] <- run_patch_model_symptom_screening_incomingphase(
      state_in = multipatchr:::state_symptoms_testing(
        s_patches = starting_state$s_patches,
        e_patches = starting_state$e_patches,
        i_a_patches = starting_state$i_a_patches,
        i_p_patches = starting_state$i_p_patches,
        i_s_patches = starting_state$i_s_patches,
        r_patches = starting_state$r_patches,
        diag_patches = starting_state$diag_patches,
        # e_diag_patches = starting_state$e_diag_patches,
        # i_a_diag_patches = starting_state$i_a_diag_patches,
        # i_p_diag_patches = starting_state$i_p_diag_patches,
        # i_s_diag_patches = starting_state$i_s_diag_patches,
        # r_diag_patches = starting_state$r_diag_patches,
        s_false_patches = starting_state$s_false_patches,
        r_false_patches = starting_state$r_false_patches,
        birth_rates = starting_state$birth_rates,
        death_rates = starting_state$death_rates,
        transmission_rates = starting_state$transmission_rates,
        transmission_rates_pilgrims = starting_state$transmission_rates_pilgrims,
        transmission_rates_pilgrims_and_atrisk = starting_state$transmission_rates_pilgrims_and_atrisk,
        asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
        presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
        prop_symptomatic = starting_state$prop_symptomatic,
        infection_rates = starting_state$infection_rates,
        symptom_rates = starting_state$symptom_rates,
        recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
        recovery_rates_sym = starting_state$recovery_rates_sym,
        testing_rates = starting_state$testing_rate,
        test_sensitivity = starting_state$test_sensitivity,
        test_specificity = starting_state$test_specificity,
        isolation_period = starting_state$isolation_period,
        movement_rate = movement_phases[[1]]
      ),
      dt = dt[1],
      nsim = 1
    )
    
    # Get the last 10 days so that we can use values of new_false_positives to determine the numbers leaving isolation later on
    final_ten_days <- model_output[[sim]][[1]][[1]][(dt[1]-9):dt[1]]
    
    final_state <- as.data.frame(model_output[[sim]][[1]][[1]][[dt[1]]])
    
    for (i in 2:length(movement)) {
      
      # If Hajj phase is short (i.e. 5 days), we will still have some people isolating at the
      # end of that period. Therefore, we need to rollover the unused values from final_ten_days.
      # We can do this as follows.
      
      threshold <- dt[2]
      
      if (i==3 & dt[2] < 10) {
        final_ten_days <- model_output[[sim]][[1]][[1]][(dt[1]-dt[2]+1):dt[1]]
        
        threshold <- 10 - dt[2]
        
      }
      
      if (i==4) {threshold <- 0} # Assumes that (dt[2] + dt[3]) > 10 days.
      # Setting threshold to 0 means we don't do quarantine release in that phase of model.
        
      
      model_output[[sim]][[i]] <- run_patch_model_screening_otherphases(
        state_in = multipatchr:::state_symptoms_testing(
          s_patches = final_state$value[final_state$variable == "susceptible"],
          e_patches = final_state$value[final_state$variable == "exposed"],
          i_a_patches = final_state$value[final_state$variable == "infected_asymptomatic"],
          i_p_patches = final_state$value[final_state$variable == "infected_presymptomatic"],
          i_s_patches = final_state$value[final_state$variable == "infected_symptomatic"],
          r_patches = final_state$value[final_state$variable == "recovered"],
          diag_patches = final_state$value[final_state$variable == "all_diagnosed"],
          # e_diag_patches = final_state$value[final_state$variable == "exposed_diagnosed"],
          # i_a_diag_patches = final_state$value[final_state$variable == "infected_asymptomatic_diagnosed"],
          # i_p_diag_patches = final_state$value[final_state$variable == "infected_presymptomatic_diagnosed"],
          # i_s_diag_patches = final_state$value[final_state$variable == "infected_symptomatic_diagnosed"],
          # r_diag_patches = final_state$value[final_state$variable == "recovered_diagnosed"],
          s_false_patches = final_state$value[final_state$variable == "susceptible_false_positive"],
          r_false_patches = final_state$value[final_state$variable == "recovered_false_positive"],
          birth_rates = starting_state$birth_rates,
          death_rates = starting_state$death_rates,
          transmission_rates = starting_state$transmission_rates,
          transmission_rates_pilgrims = starting_state$transmission_rates_pilgrims,
          transmission_rates_pilgrims_and_atrisk = starting_state$transmission_rates_pilgrims_and_atrisk,
          asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
          presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
          prop_symptomatic = starting_state$prop_symptomatic,
          infection_rates = starting_state$infection_rates,
          symptom_rates = starting_state$symptom_rates,
          recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
          recovery_rates_sym = starting_state$recovery_rates_sym,
          testing_rates = additional_testing_rates[i-1],
          test_sensitivity = starting_state$test_sensitivity,
          test_specificity = starting_state$test_specificity,
          isolation_period = starting_state$isolation_period,
          movement_rate = movement_phases[[i]]
        ),
        old_states = final_ten_days,
        old_states_threshold = threshold,
        dt = dt[i]+1, # add 1 because the first state is from the last day of previous time period
        nsim = 1
      )
      
      final_state <- as.data.frame(model_output[[sim]][[i]][[1]][[dt[i]+1]])
      
      # remove the first state from month 2 as it is repeat of last state in month 1
      model_output[[sim]][[i]] <- list(model_output[[sim]][[i]][[1]][2:(dt[i]+1)])
      
    }
    
    # New way to combine outputs (less code and works for any number of movement phases)
    output <- c()
    for (i in 1:length(movement)) {
      output <- c(output, model_output[[sim]][[i]][[1]])
    }
    
    # Remove the parameters from model output to reduce size
    output_to_keep <- c(
      "susceptible", "susceptible_false_positive",
      "exposed",
      # "exposed_diagnosed",
      "infected_asymptomatic",
      # "infected_asymptomatic_diagnosed",
      "infected_presymptomatic",
      # "infected_presymptomatic_diagnosed",
      "infected_symptomatic",
      # "infected_symptomatic_diagnosed",
      "recovered",
      # "recovered_diagnosed",
      "recovered_false_positive",
      "all_diagnosed",
      "imported_susceptible", "imported_exposed",
      "imported_infected_asymptomatic", "imported_infected_presymptomatic",
      "imported_infected_symptomatic", "imported_recovered",
      "new_exposed_diagnosed", "new_false_neg_exposed",
      "new_infected_asymptomatic_diagnosed", "new_false_neg_infected_asymptomatic",
      "new_infected_presymptomatic_diagnosed", "new_false_neg_infected_presymptomatic",
      "new_infected_symptomatic_diagnosed", "new_false_neg_infected_symptomatic",
      "new_susceptible_false_positive", "new_recovered_false_positive",
      "new_true_neg_susceptible", "new_true_neg_recovered",
      "released_s_false", "released_r_false",
      "released_exposed", "released_infected_asymptomatic",
      "released_infected_presymptomatic", "released_infected_symptomatic",
      "released_recovered"
    )
    
    output <- map(output, function(time) {
      
      map(time[["patches"]], function(patch) {
        
        patch[names(patch) %in% output_to_keep]
        
      })
    })
    
    model_output[[sim]] <- output
    
  }
  
  # model_output
  model_output <- format_model_output(model_output) %>%
    select(-sim)
  
}


run_patch_model_screening_vary_movement_copy <- function(movement, dt, sims, starting_state) {

  model_output <- vector(mode = "list", length = sims)

  for (sim in seq_len(sims)) {

    model_output[[sim]] <- vector(mode = "list", length = length(movement))

    model_output[[sim]][[1]] <- run_patch_model_screening_incomingphase(
      state_in = multipatchr:::state_symptoms_testing(
        s_patches = starting_state$s_patches,
        e_patches = starting_state$e_patches,
        i_a_patches = starting_state$i_a_patches,
        i_p_patches = starting_state$i_p_patches,
        i_s_patches = starting_state$i_s_patches,
        r_patches = starting_state$r_patches,
        e_diag_patches = starting_state$e_diag_patches,
        i_a_diag_patches = starting_state$i_a_diag_patches,
        i_p_diag_patches = starting_state$i_p_diag_patches,
        i_s_diag_patches = starting_state$i_s_diag_patches,
        r_diag_patches = starting_state$r_diag_patches,
        birth_rates = starting_state$birth_rates,
        death_rates = starting_state$death_rates,
        transmission_rates = starting_state$transmission_rates,
        asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
        presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
        prop_symptomatic = starting_state$prop_symptomatic,
        infection_rates = starting_state$infection_rates,
        symptom_rates = starting_state$symptom_rates,
        recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
        recovery_rates_sym = starting_state$recovery_rates_sym,
        testing_rates = starting_state$testing_rate,
        movement_rate = movement_phases[[1]]
      ),
      dt = dt[1],
      nsim = 1
    )
    browser()
    final_state <- as.data.frame(model_output[[sim]][[1]][[1]][[dt[1]]])

    for (i in 2:length(movement)) {

      model_output[[sim]][[i]] <- run_patch_model_screening_otherphases(
        state_in = multipatchr:::state_symptoms_testing(
          s_patches = final_state$value[final_state$variable == "susceptible"],
          e_patches = final_state$value[final_state$variable == "exposed"],
          i_a_patches = final_state$value[final_state$variable == "infected_asymptomatic"],
          i_p_patches = final_state$value[final_state$variable == "infected_presymptomatic"],
          i_s_patches = final_state$value[final_state$variable == "infected_symptomatic"],
          r_patches = final_state$value[final_state$variable == "recovered"],
          e_diag_patches = final_state$value[final_state$variable == "exposed_diagnosed"],
          i_a_diag_patches = final_state$value[final_state$variable == "infected_asymptomatic_diagnosed"],
          i_p_diag_patches = final_state$value[final_state$variable == "infected_presymptomatic_diagnosed"],
          i_s_diag_patches = final_state$value[final_state$variable == "infected_symptomatic_diagnosed"],
          r_diag_patches = final_state$value[final_state$variable == "recovered_diagnosed"],
          birth_rates = starting_state$birth_rates,
          death_rates = starting_state$death_rates,
          transmission_rates = starting_state$transmission_rates,
          asymptomatic_infectiousness = starting_state$asymptomatic_infectiousness,
          presymptomatic_infectiousness = starting_state$presymptomatic_infectiousness,
          prop_symptomatic = starting_state$prop_symptomatic,
          infection_rates = starting_state$infection_rates,
          symptom_rates = starting_state$symptom_rates,
          recovery_rates_asym = starting_state$recovery_rates_asym, # day^-1
          recovery_rates_sym = starting_state$recovery_rates_sym,
          testing_rates = starting_state$testing_rate,
          movement_rate = movement_phases[[i]]
        ),
        dt = dt[i]+1, # add 1 because the first state is from the last day of previous time period
        nsim = 1
      )

      final_state <- as.data.frame(model_output[[sim]][[i]][[1]][[dt[i]+1]])

      # remove the first state from month 2 as it is repeat of last state in month 1
      model_output[[sim]][[i]] <- list(model_output[[sim]][[i]][[1]][2:(dt[i]+1)])

    }

    # New way to combine outputs (less code and works for any number of movement phases)
    output <- c()
    for (i in 1:length(movement)) {
      output <- c(output, model_output[[sim]][[i]][[1]])
    }
    model_output[[sim]] <- output

    # Previous way to combine outputs (keeping here in case there are cases where new code doesn't work)
    # if (length(movement) == 2) {
    #   model_output[[sim]] <- c(model_output[[sim]][[1]][[1]], model_output[[sim]][[2]][[1]])
    # }
    # if (length(movement) == 3) {
    #   model_output[[sim]] <- c(model_output[[sim]][[1]][[1]],
    #                            model_output[[sim]][[2]][[1]],
    #                            model_output[[sim]][[3]][[1]])
    # }
    # if (length(movement) == 12) {
    #   model_output[[sim]] <- c(model_output[[sim]][[1]][[1]],
    #                            model_output[[sim]][[2]][[1]],
    #                            model_output[[sim]][[3]][[1]],
    #                            model_output[[sim]][[4]][[1]],
    #                            model_output[[sim]][[5]][[1]],
    #                            model_output[[sim]][[6]][[1]],
    #                            model_output[[sim]][[7]][[1]],
    #                            model_output[[sim]][[8]][[1]],
    #                            model_output[[sim]][[9]][[1]],
    #                            model_output[[sim]][[10]][[1]],
    #                            model_output[[sim]][[11]][[1]],
    #                            model_output[[sim]][[12]][[1]])
    # }

  }

  model_output

}

# Basic model functions ----

## Wrapper function for running basic SEIR model with multipatchr package ----

run_patch_model <- function(state_in, dt, nsim, seed = NULL) {

  out <- vector(mode = "list", length = nsim)
  ## Run multiple simulations.
  ## Each simulation outputs a list of length dt
  ## Each element in this list is a state object.
  for (sim in seq_len(nsim)) {
    this_sim <- vector(mode = "list", length = dt)
    this_sim[[1]] <- state_in
    if (dt > 1) {
      for (time in 2:dt) {
        this_sim[[time]] <- multipatchr:::update_ksa_state(
          state = this_sim[[time - 1]],
          dt = 1
        )
      }
    }

    out[[sim]] <- this_sim

  }

  out

}

# Wrapper for symptoms model:

## Wrapper function for running symptoms model with multipatchr package ----

run_patch_model_symptoms <- function(state_in, dt, nsim, seed = NULL) {

  out <- vector(mode = "list", length = nsim)
  ## Run multiple simulations.
  ## Each simulation outputs a list of length dt
  ## Each element in this list is a state object.
  for (sim in seq_len(nsim)) {
    this_sim <- vector(mode = "list", length = dt)
    this_sim[[1]] <- state_in
    if (dt > 1) {
      for (time in 2:dt) {
        print(time)
        this_sim[[time]] <- multipatchr:::update_ksa_state_symptoms(
          state = this_sim[[time - 1]],
          dt = 1,
          ksa_index = ksa_index
        )
      }
    }

    out[[sim]] <- this_sim

  }

  out

}


## Wrapper function for running symptoms model with screening with multipatchr package ----

run_patch_model_screening <- function(state_in, dt, nsim, seed = NULL) {

  out <- vector(mode = "list", length = nsim)
  ## Run multiple simulations.
  ## Each simulation outputs a list of length dt
  ## Each element in this list is a state object.
  for (sim in seq_len(nsim)) {
    this_sim <- vector(mode = "list", length = dt)
    this_sim[[1]] <- state_in
    if (dt > 1) {
      for (time in 2:dt) {
        print(time)
        this_sim[[time]] <- multipatchr:::update_ksa_state_symptoms_screening(
          state = this_sim[[time - 1]],
          dt = 1
        )
      }
    }

    out[[sim]] <- this_sim

  }

  out

}


## Additional wrapper functions for running symptoms model with screening with multipatchr package ----
### Use this during the phase where pilgrims arrive ----

run_patch_model_screening_incomingphase <- function(state_in, dt, nsim, seed = NULL) {

  out <- vector(mode = "list", length = nsim)
  ## Run multiple simulations.
  ## Each simulation outputs a list of length dt
  ## Each element in this list is a state object.
  for (sim in seq_len(nsim)) {
    this_sim <- vector(mode = "list", length = dt)
    this_sim[[1]] <- state_in
    if (dt > 1) {
      # for (time in 2:dt) {
      #   print(time)
      #   browser(expr = {time == 15})
      #   this_sim[[time]] <- multipatchr:::update_ksa_state_screening_incomingphase(
      #     state = this_sim[[time - 1]],
      #     dt = 1,
      #     ksa_index = ksa_index
      #   )
      # }
      for (time in 2:dt) {
        print(time)
        # browser(expr = {time == 15})
        if (time < 12) {
          this_sim[[time]] <- multipatchr:::update_ksa_state_screening_incomingphase(
            state = this_sim[[time - 1]],
            old_state = NULL,
            dt = 1,
            ksa_index = ksa_index,
            atrisk_index = atrisk_index,
            end_of_isolation_probabilities = end_of_isolation_probabilities
          )
        } else {
          this_sim[[time]] <- multipatchr:::update_ksa_state_screening_incomingphase(
            state = this_sim[[time - 1]],
            old_state = this_sim[[time - 10]], # can amend this to handle dynamic isolation periods
            dt = 1,
            ksa_index = ksa_index,
            atrisk_index = atrisk_index,
            end_of_isolation_probabilities = end_of_isolation_probabilities
          )
        }
      }
    }

    out[[sim]] <- this_sim

  }

  out

}

### Additional version for testing only symptomatic ppl ----

run_patch_model_symptom_screening_incomingphase <- function(state_in, dt, nsim, seed = NULL) {
  
  out <- vector(mode = "list", length = nsim)
  ## Run multiple simulations.
  ## Each simulation outputs a list of length dt
  ## Each element in this list is a state object.
  for (sim in seq_len(nsim)) {
    this_sim <- vector(mode = "list", length = dt)
    this_sim[[1]] <- state_in
    if (dt > 1) {
      # for (time in 2:dt) {
      #   print(time)
      #   browser(expr = {time == 15})
      #   this_sim[[time]] <- multipatchr:::update_ksa_state_screening_incomingphase(
      #     state = this_sim[[time - 1]],
      #     dt = 1,
      #     ksa_index = ksa_index
      #   )
      # }
      for (time in 2:dt) {
        print(time)
        # browser(expr = {time == 15})
        if (time < 12) {
          this_sim[[time]] <- multipatchr:::update_ksa_state_symptom_screening_incomingphase(
            state = this_sim[[time - 1]],
            old_state = NULL,
            dt = 1,
            ksa_index = ksa_index,
            atrisk_index = atrisk_index,
            end_of_isolation_probabilities = end_of_isolation_probabilities
          )
        } else {
          this_sim[[time]] <- multipatchr:::update_ksa_state_symptom_screening_incomingphase(
            state = this_sim[[time - 1]],
            old_state = this_sim[[time - 10]], # can amend this to handle dynamic isolation periods
            dt = 1,
            ksa_index = ksa_index,
            atrisk_index = atrisk_index,
            end_of_isolation_probabilities = end_of_isolation_probabilities
          )
        }
      }
    }
    
    out[[sim]] <- this_sim
    
  }
  
  out
  
}


### Use this during the phase where pilgrims are stationary or leaving ----

run_patch_model_screening_otherphases <- function(state_in, old_states, old_states_threshold,
                                                  dt, nsim, seed = NULL) {

  out <- vector(mode = "list", length = nsim)
  ## Run multiple simulations.
  ## Each simulation outputs a list of length dt
  ## Each element in this list is a state object.
  for (sim in seq_len(nsim)) {
    this_sim <- vector(mode = "list", length = dt)
    this_sim[[1]] <- state_in
    if (dt > 1) {
      for (time in 2:dt) {
        print(time)
        # browser()
        # this_sim[[time]] <- multipatchr:::update_ksa_state_screening_otherphases(
        #   state = this_sim[[time - 1]],
        #   dt = 1,
        #   ksa_index = ksa_index
        # )
        # browser() # RUN FUNCTION. STEP IN HERE. CHECK THAT THE CORRECT old_states values are available.
        if (time <= (old_states_threshold + 1)) {
          this_sim[[time]] <- multipatchr:::update_ksa_state_screening_otherphases(
            state = this_sim[[time - 1]],
            old_state = old_states[[time - 1]],
            dt = 1,
            ksa_index = ksa_index,
            atrisk_index = atrisk_index,
            end_of_isolation_probabilities = end_of_isolation_probabilities
          )
        } else {
          this_sim[[time]] <- multipatchr:::update_ksa_state_screening_otherphases(
            state = this_sim[[time - 1]],
            old_state = NULL, # can amend this to handle dynamic isolation periods
            dt = 1,
            ksa_index = ksa_index,
            atrisk_index = atrisk_index,
            end_of_isolation_probabilities = end_of_isolation_probabilities
          )
        }

      }

    }

    out[[sim]] <- this_sim

  }

  out

}


# Various processing and plotting functions ----

simulation_as_df <- function(simulation) {

  simulations <- lapply(simulation, function(x) {

    lapply(x, function(y) {

      y[["patches"]]

    })
  })

  out <- purrr::map_dfr(
    simulations,
    function(sim) {
      purrr::map_dfr(sim, function(sim) {
        x <- as.data.frame(do.call("rbind", sim))
        x$patch <- as.integer(rownames(x))
        x
      },
      .id = "time")
    },
    .id = "sim"
  )
  out$time <- as.integer(out$time)

  out <- pivot_longer(out,
                      cols = susceptible:recovery_rate,
                      names_to = "variable",
                      values_to = "value")
  out$value <- as.numeric(out$value)

  out
}

simulation_as_df_symptoms <- function(simulation) {

  # simulations <- lapply(simulation, function(x) {
  #
  #   lapply(x, function(y) {
  #
  #     y[["patches"]]
  #
  #   })
  # })
  simulations <- simulation

  out <- purrr::map_dfr(
    simulations,
    function(sim_list) {
      purrr::map_dfr(sim_list, function(sim) {
        # browser()
        purrr::map_dfr(sim, function(patch) {
          # browser()
          x <- as.data.frame(unlist(patch))
          names(x) <- "value"
          x$variable <- rownames(x)
          # Remove row names
          rownames(x) <- NULL
          # Rename columns and reorder
          x <- x[, c("variable", "value")]

        }, .id = "patch")
        # x <- as.data.frame(do.call("rbind", sim))
        # x <- bind_rows(as.data.frame(sim))
        # x$patch <- as.integer(rownames(x))
        # x
      },
      .id = "time")
    },
    .id = "sim"
  )

  # Define the order for "variable"
  variable_order <- c(
    "susceptible", "susceptible_false_positive",
    "exposed", #"exposed_diagnosed",
    "infected_asymptomatic", #"infected_asymptomatic_diagnosed",
    "infected_presymptomatic", #"infected_presymptomatic_diagnosed",
    "infected_symptomatic", #"infected_symptomatic_diagnosed",
    "recovered", #"recovered_diagnosed",
    "recovered_false_positive",
    "all_diagnosed",
    "imported_susceptible", "imported_exposed",
    "imported_infected_asymptomatic", "imported_infected_presymptomatic",
    "imported_infected_symptomatic", "imported_recovered",
    "new_exposed_diagnosed", "new_false_neg_exposed",
    "new_infected_asymptomatic_diagnosed", "new_false_neg_infected_asymptomatic",
    "new_infected_presymptomatic_diagnosed", "new_false_neg_infected_presymptomatic",
    "new_infected_symptomatic_diagnosed", "new_false_neg_infected_symptomatic",
    "new_susceptible_false_positive", "new_recovered_false_positive",
    "new_true_neg_susceptible", "new_true_neg_recovered",
    "released_s_false", "released_r_false",
    "released_exposed", "released_infected_asymptomatic",
    "released_infected_presymptomatic", "released_infected_symptomatic",
    "released_recovered"
  )

  out <- filter(out, variable %in% variable_order)

  # Complete the dataframe with all combinations of sim, time, patch, and variable
  out <- out %>%
    complete(sim, time, patch, variable = variable_order, fill = list(value = 0))

  out$variable <- factor(out$variable, levels = variable_order)
  out$time <- as.integer(out$time)
  out$patch <- as.integer(out$patch)
  out$value <- as.numeric(out$value)

  # Arrange out according to defined levels of "variable"
  out <- out %>%
    arrange(sim, time, patch, factor(variable, levels = variable_order))

  out
}

format_model_output <- function(model_output) {

  out <- simulation_as_df_symptoms(model_output)
  patches <- as.vector(patches_in_model$country_name)
  out$patch <- factor(out$patch, labels = patches)
  out

}

check_released_numbers <- function(formatted_model_output) {

  # Summarize the total value of the released variables (excluding those containing "false")
  released_sum <- formatted_model_output %>%
    filter(grepl("^released_", variable) & !grepl("false", variable)) %>%
    group_by(sim, patch, time) %>%
    summarise(total_released = sum(value, na.rm = TRUE), .groups = 'drop')

  # Summarize the total value of the new diagnosed variables at time - 10
  new_diagnosed_sum <- formatted_model_output %>%
    filter(grepl("^new_.*_diagnosed$", variable)) %>%
    mutate(time = time + 10) %>%
    group_by(sim, patch, time) %>%
    summarise(total_new_diagnosed = sum(value, na.rm = TRUE), .groups = 'drop')

  # Join the two summaries together to compare them
  comparison <- released_sum %>%
    inner_join(new_diagnosed_sum, by = c("sim", "patch", "time")) %>%
    mutate(equal = total_released == total_new_diagnosed)

  # Check if there is at least one FALSE value in the column
  if (any(comparison$equal == FALSE)) {
    print("There are instances where the number of people released does not equal the number diagnosed at the start of the isolation period. Review the output from this function.")
  } else {
    "The number of people released equals the number diagnosed at the start of the isolation period, as expected."
  }

}


check_all_diagnosed_sums <- function(formatted_model_output) {

  # Summarize the total value of the new diagnosed variables at time t
  new_diagnosed_sum <- formatted_model_output %>%
    filter(grepl("^new_.*_diagnosed$", variable)) %>%
    group_by(sim, patch, time) %>%
    summarise(total_new_diagnosed = sum(value, na.rm = TRUE), .groups = 'drop')

  # all_diagnosed values
  all_diagnosed <- formatted_model_output %>%
    filter(variable == "all_diagnosed") %>%
    group_by(sim, patch, time) %>%
    summarise(all_diagnosed = sum(value, na.rm = TRUE), .groups = 'drop')

  # all_released values
  diagnosed_released <- formatted_model_output %>%
    filter(grepl("released", variable) & !grepl("false", variable)) %>%
    group_by(sim, patch, time) %>%
    summarise(diagnosed_released = sum(value, na.rm = TRUE), .groups = 'drop')

  # Join the two summaries together to compare them
  comparison <- new_diagnosed_sum %>%
    inner_join(all_diagnosed, by = c("sim", "patch", "time")) %>%
    inner_join(diagnosed_released, by = c("sim", "patch", "time")) %>%
    mutate(lagged_all_diagnosed = lag(all_diagnosed)) %>%
    mutate(condition_met = all_diagnosed == lagged_all_diagnosed + total_new_diagnosed - diagnosed_released)

  # Check if there is at least one FALSE value in the column
  if (any(comparison$condition_met == FALSE, na.rm = T)) {
    print("There are instances where the number of people in isolation contains an error. Explore the output generated by this function.")
  } else {
    "The numbers of people in isolation are computed as expected."
  }

}

## Model output summary functions ----

### Epidemic size summaries ----

get_foreign_pilgrim_epidemic_size <- function(model_summary) {
  
  # filter to get only the pilgrim origin countries (ie no KSA patches)
  # exclude returning pilgrims that are currently susceptible
  # sum across all the disease compartments
  infection_history <- model_summary %>%
    filter(!str_detect(patch, "^KSA") & str_detect(variable, "^imported_")) %>%
    filter(variable != "imported_susceptible") %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  # find anybody left in recovered_diagnosed compartment
  leftover_diagnosed <- model_summary %>%
    filter(str_detect(patch, "^KSA_") & 
             !(patch %in% c("KSA_KSA", "KSA_AtRisk")) & variable == "all_diagnosed") %>%
    filter(time == max(time)) %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  # find anybody that was already recovered when they arrived in KSA
  recovered_before <- model_summary %>%
    filter(str_detect(patch, "^KSA_") & 
             !(patch %in% c("KSA_KSA", "KSA_AtRisk")) &
             variable == "imported_recovered") %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  infection_history - recovered_before + leftover_diagnosed
  
}


get_domestic_pilgrim_epidemic_size <- function(model_summary) {
  
  # filter to get only KSA patches
  # exclude returning pilgrims that are currently susceptible
  # sum across all the disease compartments
  infection_history <- model_summary %>%
    filter(patch == "KSA" & str_detect(variable, "^imported_")) %>%
    filter(variable != "imported_susceptible") %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  # find anybody left in recovered_diagnosed compartment
  leftover_diagnosed <- model_summary %>%
    filter(patch == "KSA_KSA" & variable == "all_diagnosed") %>%
    filter(time == max(time)) %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  # find anybody that was already recovered when they arrived at Hajj
  recovered_before <- model_summary %>%
    filter(patch == "KSA_KSA" &
             variable == "imported_recovered") %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  infection_history - recovered_before + leftover_diagnosed
  
}

get_ksa_nonpilgrim_epidemic_size <- function(model_summary) {
  
  # filter to get only KSA or KSA_AtRisk patches
  # exclude returning pilgrims that are currently susceptible
  # sum across all the disease compartments
  
  infected_compartments <- c("exposed", "infected_asymptomatic",
                             "infected_presymptomatic", "infected_symptomatic",
                             "recovered")
  
  # Find total infected
  total_infections <- model_summary %>%
    filter(time == max(time)) %>% 
    filter(patch %in% c("KSA", "KSA_AtRisk") & variable %in% infected_compartments) %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  # Find imported infections (i.e KSA pilgrims who were infected - we count these separately)
  imported_infections <- model_summary %>%
    filter(patch %in% c("KSA", "KSA_AtRisk") & str_detect(variable, "^imported_")) %>%
    filter(variable != "imported_susceptible") %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  # find anybody left in recovered_diagnosed compartment
  leftover_diagnosed <- model_summary %>%
    filter(patch == "KSA_KSA" & variable == "all_diagnosed") %>%
    filter(time == max(time)) %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  # find anybody that was already recovered when they arrived at Hajj
  recovered_before <- model_summary %>%
    filter(patch == "KSA_KSA" &
             variable == "imported_recovered") %>%
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
  total_infections - imported_infections - recovered_before + leftover_diagnosed
  
}

### Testing summaries ----

sum_model_testing_variables <- function(model_summary, variables) {
  
  model_summary %>% 
    filter(str_detect(patch, "^KSA_") & 
             patch != "KSA_AtRisk" & variable %in% variables) %>% 
    summarize(sum_value = sum(value, na.rm = TRUE)) %>%
    pull(sum_value)
  
}

get_true_positive_results <- function(model_summary) {
  
  true_pos_variables <- c("new_exposed_diagnosed",
                          "new_infected_asymptomatic_diagnosed",
                          "new_infected_presymptomatic_diagnosed",
                          "new_infected_symptomatic_diagnosed")
  
  sum_model_testing_variables(model_summary, true_pos_variables)
  
}

get_false_positive_results <- function(model_summary) {
  
  false_pos_variables <- c("new_susceptible_false_positive",
                           "new_recovered_false_positive")
  
  sum_model_testing_variables(model_summary, false_pos_variables)
  
}

get_false_negative_results <- function(model_summary) {
  
  false_neg_variables <- c("new_false_neg_exposed",
                           "new_false_neg_infected_asymptomatic",
                           "new_false_neg_infected_presymptomatic",
                           "new_false_neg_infected_symptomatic")
  
  sum_model_testing_variables(model_summary, false_neg_variables)
  
}

get_true_negative_results <- function(model_summary) {
  
  true_neg_variables <- c("new_true_neg_susceptible",
                          "new_true_neg_recovered")
  
  sum_model_testing_variables(model_summary, true_neg_variables)
  
}

get_imported_infections <- function(model_summary) {
  
  imported_infection_variables <- c("imported_exposed",
                                    "imported_infected_asymptomatic",
                                    "imported_infected_presymptomatic",
                                    "imported_infected_symptomatic")
  
  sum_model_testing_variables(model_summary, imported_infection_variables)
  
}



# get_pilgrim_epidemic_size <- function(model_summary) {
# 
#   # filter to get only the pilgrim origin countries
#   # exclude returning pilgrims that are currently susceptible
#   # sum across all the disease compartments
#   infection_history <- model_summary %>%
#     filter(!str_detect(patch, "^KSA_") & str_detect(variable, "^imported_")) %>%
#     filter(variable != "imported_susceptible") %>%
#     summarize(sum_value = sum(value, na.rm = TRUE)) %>%
#     pull(sum_value)
# 
#   # find anybody left in recovered_diagnosed compartment
#   leftover_diagnosed <- model_summary %>%
#     filter(str_detect(patch, "^KSA_") & str_detect(variable, "diagnosed")) %>%
#     filter(time == max(time)) %>%
#     summarize(sum_value = sum(value, na.rm = TRUE)) %>%
#     pull(sum_value)
# 
#   # find anybody that was already recovered when they arrived in KSA
#   recovered_before <- model_summary %>%
#     filter(str_detect(patch, "^KSA_") & variable == "imported_recovered") %>%
#     summarize(sum_value = sum(value, na.rm = TRUE)) %>%
#     pull(sum_value)
# 
#   infection_history - recovered_before + leftover_diagnosed
# 
# }
# 
# 
# get_nonpilgrim_epidemic_size <- function(model_summary) {
# 
#   # filter to get only the pilgrim origin countries
#   # exclude returning pilgrims that are currently susceptible
#   # sum across all the disease compartments
#   infections <- c("exposed", "infected_asymptomatic", "infected_presymptomatic",
#                   "infected_symptomatic", "recovered")
#   infection_history <- model_summary %>%
#     filter((patch == "KSA_AtRisk" | patch == "KSA") & variable %in% infections) %>%
#     filter(time == max(time)) %>%
#     summarize(sum_value = sum(value, na.rm = TRUE)) %>%
#     pull(sum_value)
# 
#   # find any KSA pilgrims that were (previously) infected or infectious upon return
#   infected_KSA_pilgrims <- model_summary %>%
#     filter(patch == "KSA" & str_detect(variable, "imported")) %>%
#     filter(variable != "imported_susceptible") %>%
#     summarize(sum_value = sum(value, na.rm = TRUE)) %>%
#     pull(sum_value)
# 
#   # find anybody left in recovered_diagnosed compartment
#   leftover_diagnosed <- model_summary %>%
#     filter(patch == "KSA_KSA" & str_detect(variable, "diagnosed")) %>%
#     filter(time == max(time)) %>%
#     summarize(sum_value = sum(value, na.rm = TRUE)) %>%
#     pull(sum_value)
# 
#   infection_history - infected_KSA_pilgrims + leftover_diagnosed
# 
# }


extract_parameter <- function(parameter_name, model_summary) {

  model_summary %>%
    filter(variable == parameter_name) %>%
    slice(1) %>%
    pull(value)

}


plot_simulation <- function(simulation,
                            vars = c(
                              "susceptible",
                              "exposed",
                              "infected",
                              "recovered")) {

  simulation <- dplyr::filter(simulation, variable %in% vars)
  # p <- ggplot(simulation, aes(time, value, col = variable, group = sim)) +
  #     geom_line(alpha = 0.3) +
  #     scale_color_manual(values = project_theme$seir_scale) +
  #     project_theme$theme +
  #     project_theme$legend +
  #     project_theme$axis_labels +
  #     facet_wrap(~patch)

  p <- ggplot(simulation, aes(time, value, col = variable, group = interaction(sim, variable))) +
    # geom_line(alpha = 0.3) +
    geom_line() +
    scale_color_manual(values = project_theme$seir_scale) +
    project_theme$theme +
    project_theme$legend +
    project_theme$axis_labels +
    facet_wrap(~patch, scales = "free")

  p
}

plot_infections <- function(simulation,
                            vars = c(
                              "susceptible",
                              "exposed",
                              "infected",
                              "recovered")) {

  simulation <- dplyr::filter(simulation, variable == "infected")
  # p <- ggplot(simulation, aes(time, value, col = variable, group = sim)) +
  #     geom_line(alpha = 0.3) +
  #     scale_color_manual(values = project_theme$seir_scale) +
  #     project_theme$theme +
  #     project_theme$legend +
  #     project_theme$axis_labels +
  #     facet_wrap(~patch)

  p <- ggplot(simulation, aes(time, value, col = variable, group = interaction(sim, variable))) +
    geom_line(alpha = 0.3) +
    geom_line() +
    scale_color_manual(values = project_theme$seir_scale) +
    project_theme$theme +
    project_theme$legend +
    project_theme$axis_labels +
    facet_wrap(~patch, scales = "free")

  p
}


plot_infections_symptoms <- function(simulation,
                                     vars = c(
                                       "infected_asymptomatic",
                                       "infected_presymptomatic",
                                       "infected_symptomatic")) {

  simulation <- dplyr::filter(simulation, variable %in% vars)

  p <- ggplot(simulation, aes(time, value, col = variable, group = interaction(sim, variable))) +
    # geom_line(alpha = 0.3) +
    geom_line() +
    scale_color_manual(values = project_theme_diag$screening_scale) +
    project_theme$theme +
    project_theme$legend +
    project_theme$axis_labels +
    facet_wrap(~patch, scales = "free")

  p
}

plot_diagnosed_infections <- function(simulation,
                                      vars = c(
                                        "exposed_diagnosed",
                                        "infected_asymptomatic_diagnosed",
                                        "infected_presymptomatic_diagnosed",
                                        "infected_symptomatic_diagnosed")) {

  simulation <- dplyr::filter(simulation, variable %in% vars)

  p <- ggplot(simulation, aes(time, value, col = variable, group = interaction(sim, variable))) +
    # geom_line(alpha = 0.3) +
    geom_line() +
    scale_color_manual(values = project_theme_diag$screening_scale) +
    project_theme$theme +
    project_theme$legend +
    project_theme$axis_labels +
    facet_wrap(~patch, scales = "free")

  p
}

plot_isolated_pilgrims <- function(simulation,
                                   vars = c(
                                     "susceptible_false_positive",
                                     "recovered_false_positive",
                                     "exposed_diagnosed",
                                     "infected_asymptomatic_diagnosed",
                                     "infected_presymptomatic_diagnosed",
                                     "infected_symptomatic_diagnosed")) {

  simulation <- dplyr::filter(simulation, variable %in% vars) %>%
    dplyr::filter(grepl("^KSA_", patch) & patch != "KSA_AtRisk") %>%
    group_by(sim, time, variable) %>%
    summarise(total = sum(value))

  # Calculate the sum of 'total' for each 'time'
  time_sums <- simulation %>%
    group_by(sim, time) %>%
    summarize(total = sum(total, na.rm = TRUE)) %>%
    mutate(variable = "total_isolating")

  # Add the summarized information back to the original dataframe
  combined_df <- bind_rows(
    simulation %>%
      mutate(variable = as.character(variable)),  # Ensure the variable column is character for consistent binding
    time_sums
  ) %>%
    mutate(group = case_when(
      grepl("false_positive$", variable) ~ "False positive",
      grepl("diagnosed$", variable) ~ "True positive",
      grepl("total_isolating", variable) ~ "Total isolating",
      TRUE ~ NA_character_  # In case there are other values that don't match the patterns
    )) %>%
    arrange(sim, time)

  # Set levels of factors for ordering in plot
  combined_df$variable <- factor(combined_df$variable, levels = c(
    "exposed_diagnosed",
    "infected_asymptomatic_diagnosed",
    "infected_presymptomatic_diagnosed",
    "infected_symptomatic_diagnosed",
    "susceptible_false_positive",
    "recovered_false_positive",
    "total_isolating"
  ))

  combined_df$group <- factor(combined_df$group, levels = c(
    "True positive",
    "False positive",
    "Total isolating"
  ))

  p <-
    ggplot(combined_df, aes(time, total, col = variable, group = interaction(sim, variable))) +
    geom_line() +
    # scale_color_manual(values = project_theme_diag$screening_scale) +
    project_theme$theme +
    project_theme$legend +
    # project_theme$axis_labels +
    xlab("Time (days)") +
    ylab("Number of people in isolation at time = t") +
    facet_wrap(~group, scales = "free")

  p
}

plot_undiagnosed_infections <- function(simulation,
                                        vars = c(
                                          "exposed",
                                          "infected_asymptomatic",
                                          "infected_presymptomatic",
                                          "infected_symptomatic")) {

  simulation <- dplyr::filter(simulation, variable %in% vars) %>%
    dplyr::filter(grepl("^KSA_", patch) & patch != "KSA_AtRisk") %>%
    group_by(sim, time, variable) %>%
    summarise(total = sum(value))

  # Set levels of factors for ordering in plot
  simulation$variable <- factor(simulation$variable, levels = c(
    "exposed",
    "infected_asymptomatic",
    "infected_presymptomatic",
    "infected_symptomatic"
  ))

  p <-
    ggplot(simulation, aes(time, total, col = variable, group = interaction(sim, variable))) +
    geom_line() +
    # scale_color_manual(values = project_theme_diag$screening_scale) +
    project_theme$theme +
    project_theme$legend +
    # project_theme$axis_labels +
    xlab("Time (days)") +
    ylab("Number of undiagnosed infections at time = t")

  p
}

plot_missed_infections <- function(simulation,
                                   vars = c(
                                     "exposed",
                                     "infected_asymptomatic",
                                     "infected_presymptomatic",
                                     "infected_symptomatic")) {

  search_pattern <- paste0("_", vars, collapse = "|")

  simulation <- simulation %>%
    dplyr::filter(grepl("^KSA_", patch) & patch != "KSA_AtRisk") %>%
    dplyr::filter(grepl(search_pattern, variable)) %>%
    group_by(sim, time, variable) %>%
    summarise(total = sum(value))

  untested_values <- simulation %>%
    group_by(sim, time) %>%
    summarise(
      exposed_untested = sum(total[grepl("imported_exposed", variable)]) - sum(total[grepl("new_exposed_diagnosed", variable)]) - sum(total[grepl("new_false_neg_exposed", variable)]),
      infected_asymptomatic_untested = sum(total[grepl("imported_infected_asymptomatic", variable)]) - sum(total[grepl("new_infected_asymptomatic_diagnosed", variable)]) - sum(total[grepl("new_false_neg_infected_asymptomatic", variable)]),
      infected_symptomatic_untested = sum(total[grepl("imported_infected_symptomatic", variable)]) - sum(total[grepl("new_infected_symptomatic_diagnosed", variable)]) - sum(total[grepl("new_false_neg_infected_symptomatic", variable)]),
      infected_presymptomatic_untested = sum(total[grepl("imported_infected_presymptomatic", variable)]) - sum(total[grepl("new_infected_presymptomatic_diagnosed", variable)]) - sum(total[grepl("new_false_neg_infected_presymptomatic", variable)])
    ) %>%
    pivot_longer(
      cols = starts_with("exposed") | starts_with("infected"),
      names_to = "variable",
      values_to = "total"
    )

  combined_df <- bind_rows(
    simulation %>%
      mutate(variable = as.character(variable)),  # Ensure the variable column is character for consistent binding
    untested_values
  ) %>%
    mutate(group = case_when(
      grepl("imported", variable) ~ "Imported",
      grepl("diagnosed", variable) ~ "True positive",
      grepl("false_neg", variable) ~ "False negative",
      grepl("untested", variable) ~ "Untested",
      TRUE ~ NA_character_  # In case there are other values that don't match the patterns
    )) %>%
    arrange(sim, time)

  # Pattern to extract the relevant part of the variable names
  relevant_parts_pattern <- "exposed|infected_asymptomatic|infected_symptomatic|infected_presymptomatic"

  # Use mutate to create a new column with the simplified variable names
  combined_df <- combined_df %>%
    mutate(variable_simplified = str_extract(variable, relevant_parts_pattern))

  # Set levels of factors for ordering in plot
  combined_df$group <- factor(combined_df$group, levels = c(
    "Imported",
    "True positive",
    "False negative",
    "Untested"
  ))

  combined_df$variable_simplified <- factor(combined_df$variable_simplified, levels = c(
    "exposed",
    "infected_asymptomatic",
    "infected_presymptomatic",
    "infected_symptomatic"
  ))

  p <-
    ggplot(combined_df, aes(time, total, col = variable_simplified, group = interaction(sim, variable_simplified))) +
    geom_line() +
    # scale_color_manual(values = project_theme_diag$screening_scale) +
    project_theme$theme +
    project_theme$legend +
    # project_theme$axis_labels +
    xlab("Time (days)") +
    ylab("Incidence at time = t") +
    facet_wrap(~group, scales = "free")

  p
}

project_theme <- list(

  seir_scale = c(
    "susceptible" = "#0258c6",
    "exposed" = "#1c5e34",
    "infected" = "#a20087",
    "recovered" = "#ff94b2"
  ),

  theme = ggplot2::theme_classic(base_size = 10),
  legend = ggplot2::theme(legend.title = element_blank()),
  axis_labels = ggplot2::theme(axis.title = element_blank())


)

project_theme_diag <- list(

  screening_scale = c(
    "exposed" = "#1f77b4",
    "infected_asymptomatic" = "#ff7f0e",
    "infected_presymptomatic" = "#2ca02c",
    "infected_symptomatic" = "#d62728",
    "recovered" = "#9467bd",
    "exposed_diagnosed" = "#17becf",
    "infected_asymptomatic_diagnosed" = "#bcbd22",
    "infected_presymptomatic_diagnosed" = "#e377c2",
    "infected_symptomatic_diagnosed" = "#7f7f7f",
    "recovered_diagnosed" = "#8c564b"
  ),

  theme = ggplot2::theme_classic(base_size = 10),
  legend = ggplot2::theme(legend.title = element_blank()),
  axis_labels = ggplot2::theme(axis.title = element_blank())


)
