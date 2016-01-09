# cron

This application injects a heartbeat monitor into a running LibEMP server,
and installs a sink to listen for heartbeats. The sink asks the cron service
for registered "commands" and will launch them under the 'task supervisor'.

