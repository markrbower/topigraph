get_secret_vault <- function() {
  #' Create a data iterator for MEF files
  #' Assumes that the user has already created an SSH key.
  #' 
  #' @return A link to a vault. Creates a new one if none exist.
  #' @export
  #' @examples
  #' vault <- get_secret_vault()
  library( secret )
  library( openssl )
  
  # Does the vault file already exist? If not, create it.
  vault <- file.path( "~/.secret_vault")
  if ( !file.exists( vault ) ) {
    if (dir.exists(vault)) unlink(vault) # ensure vault is empty
    create_vault(vault)
    si <- Sys.info()
    username <- si[['user']]
    add_user( username, public_key = openssl::read_pubkey( local_key() ), vault = vault )
  }
  return( vault )
}
