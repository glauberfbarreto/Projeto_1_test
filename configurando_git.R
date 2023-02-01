library(usethis)
usethis::use_git_config(# Seu nome
  user.name = "Glauber Ferreira Barreto", 
  # Seu email
  user.email = "glauberfbarreto@gmail.com")

#Abra o arquivo .Renviron usando a seguinte função:

usethis::edit_r_environ()
## Criar um novo token no GitHub:
usethis::create_github_token()
# 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

# Abra o arquivo .Renviron:
usethis::edit_r_environ()

usethis::git_sitrep()
