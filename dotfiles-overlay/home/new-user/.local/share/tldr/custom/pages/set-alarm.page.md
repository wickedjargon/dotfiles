# set-alarm

> Set alarms from the command line using systemd timers.
> Smart time parsing: past times automatically roll to the next day.
> Fires a critical notification + sound when triggered.
> Alarms persist across terminal closure (managed by systemd).
> Data: ~/.local/share/set-alarm/

- Set an alarm for a specific time (rolls to tomorrow if past):

`set-alarm {{2pm}}`

- Set an alarm with a label:

`set-alarm {{2:30pm}} "{{Take medicine}}"`

- Set an alarm using 24-hour format:

`set-alarm {{14:00}}`

- Set an alarm for a relative time from now:

`set-alarm {{30min}}`

- Set an alarm with a relative hours+minutes offset:

`set-alarm {{1hr30min}} "{{Meeting}}"`

- Set an alarm for explicitly tomorrow:

`set-alarm tomorrow {{9am}}`

- Set an alarm for a specific date:

`set-alarm {{2025-04-02}} {{2pm}}`

- List active alarms:

`set-alarm list`

- Cancel an alarm by ID:

`set-alarm cancel {{ID}}`

- Create an alarm + Google Calendar event using natural language:

`set-alarm cal "{{Dentist next Tuesday at 10am}}"`

- Create an alarm + calendar event with a duration:

`set-alarm cal "{{Lunch with Bob Friday noon for 1 hour}}"`
