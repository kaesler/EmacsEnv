/*
 *  NAME:
 *      usleep     -- This is the precision timer for Test Set
 *                    Automation. It uses the select(2) system
 *                    call to delay for the desired number of
 *                    micro-seconds. This call returns ZERO
 *                    (which is usually ignored) on successful
 *                    completion, -1 otherwise. 
 *
 *  ALGORITHM:
 *      1) We range check the passed in microseconds and log a
 *         warning message if appropriate. We then return without
 *         delay, flagging an error. 
 *      2) Load the Seconds and micro-seconds portion of the
 *         interval timer structure.
 *      3) Call select(2) with no file descriptors set, just the
 *         timer, this results in either delaying the proper
 *         ammount of time or being interupted early by a signal.
 *
 *  HISTORY:
 *      Added when the need for a subsecond timer was evident.
 *
 *  AUTHOR:
 *      Michael J. Dyer   <mike@sherlock.med.ge.com>
 */

#ifndef HAVE_USLEEP
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/types.h>

int     usleep( unsigned long int microSeconds )
{
        unsigned int            Seconds, uSec;
        int                     nfds, readfds, writefds, exceptfds;
        struct  timeval         Timer;

        nfds = readfds = writefds = exceptfds = 0;

        if( (microSeconds == (unsigned long) 0) 
                || microSeconds > (unsigned long) 4000000 )
        {
                errno = ERANGE;         /* value out of range */
                perror( "usleep time out of range ( 0 -> 4000000 ) " );
                return -1;
        }

        Seconds = microSeconds / (unsigned long) 1000000;
        uSec    = microSeconds % (unsigned long) 1000000;

        Timer.tv_sec            = Seconds;
        Timer.tv_usec           = uSec;

        if( select( nfds, &readfds, &writefds, &exceptfds, &Timer ) < 0 )
        {
                perror( "usleep (select) failed" );
                return -1;
        }

        return 0;
}
#else
/* this is to avoid a "object file has no symbol" error/warning.
*/
static int local_junk;
#endif
