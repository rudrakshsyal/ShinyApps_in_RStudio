## Message Menu dropdown - Top right corner

message_menu <- function(id, label = "Messages"){
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Daily Transactions",
                 message = "Just 0.45% dip on Christmas."
               ),
               messageItem(
                 from = "GMV",
                 message = "4.9% rise in Q3 against 134 Cr. in Sept.",
                 icon = icon("question"),
                 time = "16:40"
               ),
               messageItem(
                 from = "Profile Page Traffic",
                 message = "DCH Traffic saw 24% rise from Aug'17.",
                 icon = icon("life-ring"),
                 time = "2018-01-11"
               )
  )}