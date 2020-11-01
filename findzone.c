/* The Ada Structured Library - A set of container classes and general
     tools for use with Ada95.
   Copyright (C) 2001  Corey Minyard (minyard@acm.org)
   
   This library is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this library; if not, write to the Free Software Foundation, Inc.,
   59 Temple Place - Suite 330, Boston, MA 02111-1307, USA

   As a special exception, if other files instantiate generics from this
   unit, or you link this unit with other files to produce an executable,
   this unit does not by itself cause the resulting executable to be
   covered by the GNU General Public License.  This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU Public License. */

#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>

static struct {
	char *region;
	char *zone;
} zonetab[] = {
#include "zonetab.h"
	{ NULL, NULL }
};

/* Check the zone name against the zone data we have to try to match it. */
extern int try_zone(char *zone_name,
		    int  dst_active,
		    long gmt_offset,
		    char *off_id,
		    char *on_id);

/* Try to figure out if DST is active in the current year by going through
   all the months and trying it. */
static int
is_dst_active(void)
{
	struct tm t1, t2;
	time_t    now;
	int       i;

	now = time(NULL);
	t1 = *localtime(&now);
	t1.tm_sec = 0;
	t1.tm_min = 0;
	t1.tm_hour = 0;
	t1.tm_mday = 1;
	for (i=0; i<12; i++) {
		t1.tm_mon = i;
		now = mktime(&t1);
		t2 = *localtime(&now);
		if (t2.tm_isdst) {
			return 1;
		}
	}

	return 0;
}

void
findlocalzone(void)
{
	char *val;
	char *region;
	int  i;
	int  dst_active = is_dst_active();
	char tmp[100];

	tzset();

	/* First try the TZ variable to see if it is a valid zone. */
	val = getenv("TZ");
	if ((val != NULL) && (try_zone(val,
				       dst_active,
				       -timezone,
				       tzname[0],
				       tzname[1])))
	{
		return;
	}

	/* Now try combining the zone names and offset together. */
	if (daylight) {
		sprintf(tmp, "%s%ld%s", tzname[0], timezone/3600, tzname[1]);
	} else {
		sprintf(tmp, "%s%ld", tzname[0], timezone/3600);
	}
	/* First try the TZ variable to see if it is a valid zone. */
	if (try_zone(tmp,
		     dst_active,
		     -timezone,
		     tzname[0],
		     tzname[1]))
	{
		return;
	}

	/* No zone yet, so try to derive it from the region. */
	val = getenv("LANG");
	if (val == NULL) {
		val = "en_US";
	}
	region = val;
	while (*region != '_') {
		if (*region == '\0') {
			region = "_US";
			break;
		}
		region++;
	}
	region++;
	/* First look in the zone table to see if the zone is there. */
	for (i=0; ; i++) {
		if (zonetab[i].region == NULL) break;

		if (strcmp(region, zonetab[i].region) == 0) {
			if (try_zone(zonetab[i].zone,
				     dst_active,
				     -timezone,
				     tzname[0],
				     tzname[1]))
			{
				return;
			}
		}
	}

	/* Well, we have to default to a fail-safe. */
	try_zone("UTC", 0, 0, "UTC", "UTC");
}

struct c_portable_time {
    long year;
    long second_of_year;
    long microsecond;
};

/* This algorithm was taken from glibc. */
#define DIV(a, b) ((a) / (b) - ((a) % (b) < 0))
#define LEAPS_THRU_END_OF(y) (DIV (y, 4) - DIV (y, 100) + DIV (y, 400))
#define	SECS_PER_HOUR	(60 * 60)
#define	SECS_PER_DAY	(SECS_PER_HOUR * 24)

int
is_leap_year(int year)
{
    return (((year) % 4) == 0 && (((year) % 100 != 0) || ((year) % 400 == 0)));
}

int
days_in_year(int year)
{
    if (is_leap_year(year))
	return 366;
    else
	return 365;	
}

void
asl_date_time_get_curr_portable_time(struct c_portable_time *t)
{
    struct timeval tv;
    long           days;
    long           seconds;

    gettimeofday(&tv, NULL);

    t->year = 1970;
    days = tv.tv_sec / SECS_PER_DAY;
    seconds = tv.tv_sec % SECS_PER_DAY;

    while ((days < 0) || (days >= days_in_year(t->year)))
    {
	/* Guess a corrected year, assuming 365 days per year.  */
	long int guess = t->year + days / 365 - (days % 365 < 0);
	
	/* Adjust DAYS and Y to match the guessed year.  */
	days -= ((guess - t->year) * 365
		 + LEAPS_THRU_END_OF (guess - 1)
		 - LEAPS_THRU_END_OF (t->year - 1));
	t->year = guess;
    }

    t->second_of_year = (days * SECS_PER_DAY) + seconds;
    t->microsecond = tv.tv_usec;
}
