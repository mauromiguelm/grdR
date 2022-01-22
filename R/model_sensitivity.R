model_growth <- function(cell_tz, t, t_onset, k, maxeff, halfeff, conc, hill){

  maxeff <- ifelse(t_onset > t, 0, maxeff)

  if(t >= t_onset){

    cell_tz <- cell_tz * exp( ( t_onset * k ) )

    t <- t - t_onset

    return( cell_tz * exp( ( t * k *( 1 - ( (maxeff*conc ** hill )/ (halfeff ** hill + conc ** hill) ) ) ) ) )

  }else{

    return( cell_tz * exp( ( t * k *( 1 - ( (maxeff*conc ** hill )/ (halfeff ** hill  + conc ** hill) ) ) ) ) )

  }
}
