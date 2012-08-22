package org.randi3.utility

import javax.mail._
import javax.mail.internet._
import java.util.Properties

trait MailSenderComponent extends Utility {

  val mailSender: MailSender

  class MailSender(smtpHost: String, port: String, smtpAuth: Boolean, username: String, password: String, ssl: Boolean, from: String) {

    def sendMessage(to: String, cc: String, bcc: String, subject: String, content: String) {
      try {

        val props = new Properties()
        props.put("mail.smtp.host", smtpHost)
        props.put("mail.smtp.port", port)

        if (smtpAuth) {
          props.put("mail.smtp.auth", "true")
        } else {
          props.put("mail.smtp.auth", "false")
        }

        if (ssl) {
          props.put("mail.smtp.socketFactory.port", port)
          props.put("mail.smtp.socketFactory.class",
            "javax.net.ssl.SSLSocketFactory")
        }

        val session = if (smtpAuth) {
          Session.getInstance(props,
            new javax.mail.Authenticator {
              override def getPasswordAuthentication: PasswordAuthentication = {
                new PasswordAuthentication(username, password)
              }
            })
        } else {
          Session.getInstance(props, null)
        }

        val message = new MimeMessage(session)

        // Set the from, to, subject, body text
        message.setFrom(new InternetAddress(from))
        message.setRecipients(Message.RecipientType.TO, to)
        message.setRecipients(Message.RecipientType.CC, cc)
        message.setRecipients(Message.RecipientType.BCC, bcc)

        message.setSubject(subject)
        message.setContent(content, "text/html" )

        Transport.send(message)
      } catch {
        case e: Exception =>  logError(e)
      }
    }


  }

}