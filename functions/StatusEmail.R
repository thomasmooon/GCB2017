StatusEmail <- function(subject, body) {
sender <- "thomas.gerlach@ucb.com"
recipients <- c("thomas.gerlach@ucb.com")
mailR::send.mail(from = sender,
          to = recipients,
          subject = subject,
          body = body,
          smtp = list(host.name = "mail.ucb.com"),
          send = TRUE)
}
