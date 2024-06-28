/** Function to shift the current view to the left/right
 *
 * @param: "arg->i" stores the number of tags to shift right (positive value)
 *          or left (negative value)
 */
void
shiftview(const Arg *arg)
{
	Arg shifted;
	Client *c;
	unsigned int tagmask = 0;

	for (c = selmon->clients; c; c = c->next)
		if (!(c->tags & SPTAGMASK))
			tagmask = tagmask | c->tags;

	shifted.ui = selmon->tagset[selmon->seltags] & ~SPTAGMASK;
	if (arg->i > 0) /* left circular shift */
		do {
			shifted.ui = (shifted.ui << arg->i)
			   | (shifted.ui >> (LENGTH(tags) - arg->i));
			shifted.ui &= ~SPTAGMASK;
		} while (tagmask && !(shifted.ui & tagmask));
	else /* right circular shift */
		do {
			shifted.ui = (shifted.ui >> (- arg->i)
			   | shifted.ui << (LENGTH(tags) + arg->i));
			shifted.ui &= ~SPTAGMASK;
		} while (tagmask && !(shifted.ui & tagmask));

	view(&shifted);
}

void
shifttag(const Arg *arg)
{
	Arg a;
	Client *c;
	unsigned visible = 0;
	int i = arg->i;
	int count = 0;
	int nextseltags, curseltags = selmon->tagset[selmon->seltags];

	do {
		if(i > 0) // left circular shift
			nextseltags = (curseltags << i) | (curseltags >> (LENGTH(tags) - i));

		else // right circular shift
			nextseltags = curseltags >> (- i) | (curseltags << (LENGTH(tags) + i));

                // Check if tag is visible
		for (c = selmon->clients; c && !visible; c = c->next)
			if (nextseltags & c->tags) {
				visible = 1;
				break;
			}
		i += arg->i;
	} while (!visible && ++count < 10);

	if (count < 10) {
		a.i = nextseltags;
		tag(&a);
	}
}

void
view_adjacent(const Arg *arg)
{
  int i, curtags;
  int seltag = 0;
  Arg a;

  curtags = selmon->tagset[selmon->seltags];
  for(i = 0; i < LENGTH(tags); i++)
    if(curtags & (1 << i)){
      seltag = i;
      break;
    }
  seltag = (seltag + arg->i) % (int)LENGTH(tags);
  if(seltag < 0)
    seltag += LENGTH(tags);

  /* ff: don't cycle/wrap around from 1 to last, or last to 1 */
  if(arg->i == -1) {
    if(!(curtags == 1)) {
      a.i = (1 << seltag);
    } else {
      a.i = 1;
    }
  } else {
    if(!(curtags == (1 << ((int)LENGTH(tags) - 1)) && (1 << seltag) == 1)) {
      a.i = (1 << seltag);
    } else {
      a.i = (1 << ((int)LENGTH(tags) - 1));
    }
  }

  view(&a);
}
