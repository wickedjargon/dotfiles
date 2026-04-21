# sched

> Set alarms/reminders from the command line using systemd timers.
> Smart time parsing: past times automatically roll to the next day.
> Fires a critical notification + sound when triggered.
> Alarms persist across terminal closure (managed by systemd).
> Data: ~/.local/share/sched/

- Set an alarm for a specific time (rolls to tomorrow if past):

`sched {{2pm}}`

- Set an alarm with a label:

`sched {{2:30pm}} "{{Take medicine}}"`

- Set an alarm using 24-hour format:

`sched {{14:00}}`

- Set an alarm for a relative time from now:

`sched {{30min}}`

- Set an alarm with a relative hours+minutes offset:

`sched {{1hr30min}} "{{Meeting}}"`

- Set an alarm for explicitly tomorrow:

`sched tomorrow {{9am}}`

- Set an alarm for a specific date:

`sched {{2025-04-02}} {{2pm}}`

- List active alarms:

`sched list`

- Delete an alarm by ID:

`sched del {{ID}}`

- Create an alarm + Google Calendar 1-hour block event using natural language:

`sched cal "{{Dentist next Tuesday at 10am}}"`

- Create an alarm + Google Calendar zero-duration reminder using natural language:

`sched rem "{{Get laundry today right now}}"`
