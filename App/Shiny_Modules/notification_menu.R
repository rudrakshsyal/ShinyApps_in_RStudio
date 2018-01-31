## Notification Menu dropdown - Top right corner

notification_menu <- function(id, label = "Notifications"){
  dropdownMenu(
  type = "notifications",
  
  notificationItem(
    text = (paste0("Last Updated on - ", update_time)),
    icon = icon("welcome"),
    status = "warning"
  ))}