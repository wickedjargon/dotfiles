# google-calendar

> Manage Google Calendar events from the command line.
> Uses natural language for creating and editing events.
> Auth is cached after first run (browser opens for OAuth consent).
> Config: ~/.config/google-calendar/

- Create an event using natural language:

`google-calendar add "{{Dentist next Tuesday at 10am}}"`

- Create an event with a duration:

`google-calendar add "{{Lunch with Bob Friday noon for 1 hour}}"`

- List today's remaining events:

`google-calendar list`

- List events for the next N days:

`google-calendar list --days {{7}}`

- Show full details of an event:

`google-calendar show {{event_id}}`

- Edit an event (replaces it with new text):

`google-calendar edit {{event_id}} "{{Dentist Wednesday at 11am}}"`

- Delete an event:

`google-calendar delete {{event_id}}`
