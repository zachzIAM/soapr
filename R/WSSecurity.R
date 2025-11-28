#' WS-Security Helper
#'
#' Provides methods for generating WS-Security headers.
#'
#' @importFrom xml2 xml_new_root xml_add_child xml_set_attr xml_set_text
#' @export
WSSecurity <- R6::R6Class(
    "WSSecurity",
    public = list(
        initialize = function() {}
    ),
    active = list(),
    private = list(),
    cloneable = FALSE
)

#' Generate a WS-Security UsernameToken Header
#'
#' @param username The username.
#' @param password The password.
#' @param password_type The password type (default: PasswordText).
#' @return An xml_node representing the Security header.
#' @export
wss_username_token <- function(
    username,
    password,
    password_type = "PasswordText"
) {
    wsse_ns <- "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"
    wsu_ns <- "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"

    security <- xml2::xml_new_root(
        "wsse:Security",
        "xmlns:wsse" = wsse_ns,
        "xmlns:wsu" = wsu_ns
    )

    token <- xml2::xml_add_child(security, "wsse:UsernameToken")
    xml2::xml_add_child(token, "wsse:Username", username)

    pw_node <- xml2::xml_add_child(token, "wsse:Password", password)
    xml2::xml_set_attr(
        pw_node,
        "Type",
        sprintf(
            "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#%s",
            password_type
        )
    )

    return(security)
}
