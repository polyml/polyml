/*
    Title:      Operating Specific functions: Unix version.

	Copyright (c) 2000 David C. J. Matthews
	Portions of this code are derived from the original stream io
	package copyright CUTS 1983-2000.

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
/* #include <malloc.h> */
#include <stdlib.h>
#include <limits.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/termios.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <signal.h>

#include "globals.h"
#include "arb.h"
#include "run_time.h"
#include "io_internal.h"
#include "sys.h"
#include "proper_io.h"
#include "diagnostics.h"
#include "machine_dep.h"
#include "os_specific.h"
#include "gc.h"
#include "processes.h"
#include "mpoly.h"
#include "sighandler.h"

#define STREAMID(x) (*DEREFSTREAMHANDLE(x))

#define SAVE(x) push_to_save_vec((word)(x))
#define ALLOC(n) alloc_and_save(n)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

/* This is used to simulate the alarm clock maintained by "alarm".  Since we
   use SIGALRM for process scheduling we have to handle this internally.  It is
   only approximately accurate and depends on the frequency of the process
   scheduling clock. */
static struct timeval alarmclock = { 0, 0 };

/* Table of constants returned by call 4. */
static int unixConstVec[] =
{
	/* Error codes. */
	E2BIG, /* 0 */
	EACCES,
	EAGAIN,
	EBADF,
#ifdef EBADMSG
/* This is not defined in FreeBSD. */
	EBADMSG,
#else
	0,
#endif
	EBUSY,
#ifdef ECANCELED
/* This is not defined in Linux. */
	ECANCELED,
#else
	0, /* Perhaps some other value. */
#endif
	ECHILD,
	EDEADLK,
	EDOM,
	EEXIST,
	EFAULT,
	EFBIG,
	EINPROGRESS,
	EINTR,
	EINVAL,
	EIO,
	EISDIR,
	ELOOP,
	EMFILE,
	EMLINK, /* 20 */
	EMSGSIZE,
	ENAMETOOLONG,
	ENFILE,
	ENODEV,
	ENOENT,
	ENOEXEC,
	ENOLCK,
	ENOMEM,
	ENOSPC,
	ENOSYS,
	ENOTDIR,
	ENOTEMPTY,
#ifdef ENOTSUP
/* Not defined in Linux. */
	ENOTSUP,
#else
	0,
#endif
	ENOTTY,
	ENXIO,
	EPERM,
	EPIPE,
	ERANGE,
	EROFS,
	ESPIPE,
	ESRCH,
	EXDEV, /* 42 */

	/* Signals. */
	SIGABRT, /* 43 */
 	SIGALRM,
	SIGBUS,
	SIGFPE,
	SIGHUP,
	SIGILL,
	SIGINT,
	SIGKILL,
	SIGPIPE,
	SIGQUIT,
	SIGSEGV,
	SIGTERM,
	SIGUSR1,
	SIGUSR2,
	SIGCHLD,
	SIGCONT,
	SIGSTOP,
	SIGTSTP,
	SIGTTIN,
	SIGTTOU, /* 62 */

	/* Open flags. */
	O_RDONLY, /* 63 */
	O_WRONLY,
	O_RDWR,
	O_APPEND,
	O_EXCL,
	O_NOCTTY,
	O_NONBLOCK,
#ifdef O_SYNC
	O_SYNC, /* Not defined in FreeBSD. */
#else
	0,
#endif
	O_TRUNC, /* 71 */

	/* TTY: Special characters. */
	VEOF, /* 72 */
	VEOL,
	VERASE,
	VINTR,
	VKILL,
	VMIN,
	VQUIT,
	VSUSP,
	VTIME,
	VSTART,
	VSTOP,
	NCCS, /* 83 */

	/* TTY: Input mode. */
	BRKINT, /* 84 */
	ICRNL,
	IGNBRK,
	IGNCR,
	IGNPAR,
	INLCR,
	INPCK,
	ISTRIP,
	IXOFF,
	IXON,
	PARMRK, /* 94 */

	/* TTY: Output mode. */
	OPOST, /* 95 */

	/* TTY: Control modes. */
	CLOCAL, /* 96 */
	CREAD,
	CS5,
	CS6,
	CS7,
	CS8,
	CSIZE,
	CSTOPB,
	HUPCL,
	PARENB,
	PARODD, /* 106 */

	/* TTY: Local modes. */
	ECHO, /* 107 */
	ECHOE,
	ECHOK,
	ECHONL,
	ICANON,
	IEXTEN,
	ISIG,
	NOFLSH,
	TOSTOP, /* 115 */

	/* TTY: Speeds. */
	B0, /* 116 */
	B50,
	B75,
	B110,
	B134,
	B150,
	B200,
	B300,
	B600,
	B1200,
	B1800,
	B2400,
	B4800,
	B9600,
	B19200,
	B38400, /* 131 */

	/* FD flags. */
	FD_CLOEXEC, /* 132 */

	/* Wait flags. */
	WUNTRACED, /* 133 */
	WNOHANG, /* 134 */

	/* tcsetattr flags. */
	TCSANOW, /* 135 */
	TCSADRAIN,
	TCSAFLUSH,
	/* tcflow flags. */
	TCOOFF, /* 138 */
	TCOON,
	TCIOFF,
	TCION,
	/* tcflush flags. */
	TCIFLUSH, /* 142 */
	TCOFLUSH,
	TCIOFLUSH,
	
	/* File permissions. */
	S_IRUSR, /* 145 */
	S_IWUSR,
	S_IXUSR,
	S_IRGRP,
	S_IWGRP,
	S_IXGRP,
	S_IROTH,
	S_IWOTH,
	S_IXOTH,
	S_ISUID,
	S_ISGID, /* 155 */

	/* Bits for access function. */
	R_OK, /* 156 */
	W_OK,
	X_OK,
	F_OK, /* 159 */
	
	/* Values for lseek. */
	SEEK_SET, /* 160 */
	SEEK_CUR,
	SEEK_END, /* 162 */

	/* Values for lock types. */
	F_RDLCK, /* 163 */
	F_WRLCK,
	F_UNLCK, /* 165 */

	/* Mask for file access. */
	O_ACCMODE, /* 166 */
};

/* Auxiliary functions which implement the more complex cases. */
static Handle waitForProcess(Handle args);
static Handle makePasswordEntry(struct passwd *pw);
static Handle makeGroupEntry(struct group *grp);
static Handle getUname(void);
static Handle getSysConf(Handle args);
static Handle getTTYattrs(Handle args);
static Handle setTTYattrs(Handle args);
static Handle getStatInfo(struct stat *buf);
static int findPathVar(Handle hString);
static Handle lockCommand(int cmd, Handle args);

Handle OS_spec_dispatch_c(Handle args, Handle code)
{
	int c = get_C_long(DEREFWORDHANDLE(code));
	switch (c)
	{
	case 0: /* Return our OS type.  Not in any structure. */
		return Make_arbitrary_precision(0); /* 0 for Unix. */

	case 4: /* Return a constant. */
		{
			int i = get_C_long(DEREFWORDHANDLE(args));
			if (i < 0 || i >= sizeof(unixConstVec)/sizeof(unixConstVec[0]))
				raise_syscall("Invalid index", 0);
			return Make_unsigned(unixConstVec[i]);
		}

	case 5: /* fork. */
		{
			pid_t pid = fork();
			if (pid < 0) raise_syscall("fork failed", errno);
			return Make_unsigned(pid);
		}

	case 6: /* kill */
		{
			int pid = get_C_long(DEREFHANDLE(args)[0]);
			int sig = get_C_long(DEREFHANDLE(args)[1]);
			if (kill(pid, sig) < 0) raise_syscall("kill failed", errno);
			return Make_unsigned(0);
		}

	case 7: /* get process id */
		{
			pid_t pid = getpid();
			if (pid < 0) raise_syscall("getpid failed", errno);
			return Make_unsigned(pid);
		}

	case 8: /* get process id of parent */
		{
			pid_t pid = getppid();
			if (pid < 0) raise_syscall("getppid failed", errno);
			return Make_unsigned(pid);
		}

	case 9: /* get real user id */
		{
			uid_t uid = getuid();
			if (uid < 0) raise_syscall("getuid failed", errno);
			return Make_unsigned(uid);
		}

	case 10: /* get effective user id */
		{
			uid_t uid = geteuid();
			if (uid < 0) raise_syscall("geteuid failed", errno);
			return Make_unsigned(uid);
		}

	case 11: /* get real group id */
		{
			gid_t gid = getgid();
			if (gid < 0) raise_syscall("getgid failed", errno);
			return Make_unsigned(gid);
		}

	case 12: /* get effective group id */
		{
			gid_t gid = getegid();
			if (gid < 0) raise_syscall("getegid failed", errno);
			return Make_unsigned(gid);
		}

	case 13: /* Return process group */
		{
			pid_t pid = getpgrp();
			if (pid < 0) raise_syscall("getpgrp failed", errno);
			return Make_unsigned(pid);
		}

	case 14: /* Wait for child process to terminate. */
		return waitForProcess(args);

	case 15: /* Unpack a process result. */
		{
			int resType, resVal;
			Handle result,  typeHandle, resHandle;
			int status = get_C_long(DEREFWORDHANDLE(args));
			if (WIFEXITED(status))
			{
				resType = 1;
				resVal = WEXITSTATUS(status);
			}
			else if (WIFSIGNALED(status))
			{
				resType = 2;
				resVal = WTERMSIG(status);
			}
			else if (WIFSTOPPED(status))
			{
				resType = 3;
				resVal = WSTOPSIG(status);
			}
			else { /* ?? */
				resType = 0;
				resVal = 0;
			}
			typeHandle = Make_arbitrary_precision(resType);
			resHandle = Make_arbitrary_precision(resVal);

			result = ALLOC(2);
			DEREFHANDLE(result)[0] = DEREFWORDHANDLE(typeHandle);
			DEREFHANDLE(result)[1] = DEREFWORDHANDLE(resHandle);
			return result;
		}

	case 16: /* Pack up a process result.  The inverse of the previous call. */
		{
			int resType = get_C_long(DEREFHANDLE(args)[0]);
			int resVal = get_C_long(DEREFHANDLE(args)[1]);
			int result = 0;
			switch (resType)
			{
			case 1: /* Exited */ result = resVal << 8; break;
			case 2: /* Signalled */ result = resVal; break;
			case 3: /* Stopped */ result = (resVal << 8) | 0177;
			}
			return Make_unsigned(result);
		}

	case 17: /* Run a new executable. */
		{
			char *path = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			char **argl = stringListToVector((Handle)&DEREFHANDLE(args)[1]);
			int err;
			execv(path, argl);
			err = errno;
			/* We only get here if there's been an error. */
			free(path);
			freeStringVector(argl);
			raise_syscall("execv failed", err);
		}

	case 18: /* Run a new executable with given environment. */
		{
			char *path = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			char **argl = stringListToVector((Handle)&DEREFHANDLE(args)[1]);
			char **envl = stringListToVector((Handle)&DEREFHANDLE(args)[2]);
			int err;
			execve(path, argl, envl);
			err = errno;
			/* We only get here if there's been an error. */
			free(path);
			freeStringVector(argl);
			freeStringVector(envl);
			raise_syscall("execve failed", err);
		}

	case 19: /* Run a new executable using PATH environment variable. */
		{
			char *path = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			char **argl = stringListToVector((Handle)&DEREFHANDLE(args)[1]);
			int err;
			execvp(path, argl);
			err = errno;
			/* We only get here if there's been an error. */
			free(path);
			freeStringVector(argl);
			raise_syscall("execvp failed", err);
		}

	case 20: /* Sets an alarm and returns the current alarm time.  The caller (within
		    the Posix structure) converts these from/to relative times. */
		{
			/* We have a value in microseconds.  We need to split
			   it into seconds and microseconds. */
			Handle hTime = args;
			Handle hMillion = Make_arbitrary_precision(1000000);
			int secs = get_C_long(DEREFWORDHANDLE(div_longc(hMillion, hTime)));
			int usecs = get_C_long(DEREFWORDHANDLE(rem_longc(hMillion, hTime)));
			Handle result = /* Return the previous setting. */
				Make_arb_from_pair_scaled(alarmclock.tv_sec,
						alarmclock.tv_usec, 1000000);
			/* Store the absolute time. */
			alarmclock.tv_sec = secs;
			alarmclock.tv_usec = usecs;
			return result;
		}

	case 21: /* Pause until signal. */
		{
			/* This never returns.  When a signal is handled it will
			   be interrupted. */
			block_and_restart(-1, 1 /* Interruptable. */, POLY_SYS_os_specific);
		}

	case 22: /* Sleep until given time or until a signal.  Note: this is called
		    with an absolute time as an argument and returns a relative time as
		    result.  This RTS call is tried repeatedly until either the time has
		    expired or a signal has occurred. */
		{
			struct timeval tv;
			struct timezone tz;
			/* We have a value in microseconds.  We need to split
			   it into seconds and microseconds. */
			Handle hTime = args;
			Handle hMillion = Make_arbitrary_precision(1000000);
			unsigned secs = get_C_ulong(DEREFWORDHANDLE(div_longc(hMillion, hTime)));
			unsigned usecs = get_C_ulong(DEREFWORDHANDLE(rem_longc(hMillion, hTime)));
			/* Has the time expired? */
			if (gettimeofday(&tv, &tz) != 0)
				raise_syscall("gettimeofday failed", errno);
			/* If the timeout time is earlier than the current time
			   we must return, otherwise we block.  This can be interrupted
			   by a signal. */
			if (tv.tv_sec < secs || (tv.tv_sec == secs && tv.tv_usec < usecs))
				block_and_restart(-1, 1 /* Interruptable. */, POLY_SYS_os_specific);
			return Make_arbitrary_precision(0);
		}
	
	case 23: /* Set uid. */
		{
			uid_t uid = get_C_long(DEREFWORDHANDLE(args));
			if (setuid(uid) != 0) raise_syscall("setuid failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 24: /* Set gid. */
		{
			gid_t gid = get_C_long(DEREFWORDHANDLE(args));
			if (setgid(gid) != 0) raise_syscall("setgid failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 25: /* Get group list. */
		{
			gid_t groups[NGROUPS_MAX];
			Handle list  = SAVE(nil_value);
			Handle saved = mark_save_vec();
			int ngroups = getgroups(NGROUPS_MAX, groups);
			if (ngroups < 0) raise_syscall("getgroups failed", errno);
			saved = mark_save_vec();

			/* It's simplest to process the integers in reverse order */
			while (--ngroups >= 0)
			{
				Handle value = Make_arbitrary_precision(groups[ngroups]);
				Handle next  = ALLOC(SIZEOF(ML_Cons_Cell));
				DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
				DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
				DEREFHANDLE(list) = DEREFHANDLE(next);
				reset_save_vec(saved);
			}
			return list;
		}

	case 26: /* Get login name. */
		{
			char *login = getlogin();
			if (login == 0) raise_syscall("getlogin failed", errno);
			return SAVE(C_string_to_Poly(login));
		}
		
	case 27: /* Set sid */
		{
			pid_t pid = setsid();
			if (pid < 0) raise_syscall("setsid failed", errno);
			return Make_arbitrary_precision(pid);
		}

	case 28: /* Set process group. */
		{
			pid_t pid = get_C_long(DEREFHANDLE(args)[0]);
			pid_t pgid = get_C_long(DEREFHANDLE(args)[1]);
			if (setpgid(pid, pgid) < 0 ) raise_syscall("setpgid failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 29: /* uname */ return getUname();

	case 30: /* Get controlling terminal. */
		{
			char *term = ctermid(0);
			/* Can this generate an error? */
			if (term == 0) raise_syscall("ctermid failed", errno);
			return SAVE(C_string_to_Poly(term));
		}

	case 31: /* Get terminal name for file descriptor. */
		{
			PIOSTRUCT str = get_stream(args);
			char *term;
			if (str == NULL) raise_syscall("Stream is closed", EBADF);
			term = ttyname(str->device.ioDesc);
			if (term == 0) raise_syscall("ttyname failed", errno);
			return SAVE(C_string_to_Poly(term));
		}

	case 32: /* Test if file descriptor is a terminal.  Returns false if
		    the stream is closed. */
		{
			PIOSTRUCT str = get_stream(args);
			if (str != NULL && isatty(str->device.ioDesc))
				return Make_arbitrary_precision(1);
			else return Make_arbitrary_precision(0);
		}

	case 33: /* sysconf. */
		return getSysConf(args);

		/* Filesys entries. */
	case 50: /* Set the file creation mask and return the old one. */
		{
			mode_t mode = get_C_ulong(DEREFWORDHANDLE(args));
			return Make_unsigned(umask(mode));
		}

	case 51: /* Create a hard link. */
		{
			char *old = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			char *new = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[1]);
			int err, res;
			res = link(old, new);
			err = errno; /* Save the error result in case free changes it. */
			free(old);
			free(new);
			if (res < 0) raise_syscall("link failed", err);
			return Make_arbitrary_precision(0);
		}

	case 52: /* Create a directory.  There is an OS-independent version in
		    basicio which uses a default creation mode. */
		{
			char *name = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			mode_t mode = get_C_long(DEREFHANDLE(args)[1]);
			int err, res;
			res = mkdir(name, mode);
			err = errno; /* Save the error result in case free changes it. */
			free(name);
			if (res < 0) raise_syscall("mkdir failed", err);
			return Make_arbitrary_precision(0);
		}

	case 53: /* Create a fifo. */
		{
			char *name = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			mode_t mode = get_C_long(DEREFHANDLE(args)[1]);
			int err, res;
			res = mkfifo(name, mode);
			err = errno; /* Save the error result in case free changes it. */
			free(name);
			if (res < 0) raise_syscall("mkfifo failed", err);
			return Make_arbitrary_precision(0);
		}

	case 54: /* Create a symbolic link. */
		{
			char *old = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			char *new = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[1]);
			int err, res;
			res = symlink(old, new);
			err = errno; /* Save the error result in case free changes it. */
			free(old);
			free(new);
			if (res < 0) raise_syscall("link failed", err);
			return Make_arbitrary_precision(0);
		}

	case 55: /* Get information about a file. */
		{
			struct stat buf;
			int res, err;
			char *name = Poly_string_to_C_alloc(DEREFSTRINGHANDLE(args));
			res = stat(name, &buf);
			err = errno;
			free(name);
			if (res < 0) raise_syscall("stat failed", err);
			return getStatInfo(&buf);
		}

	case 56: /* Get information about a symbolic link. */
		{
			struct stat buf;
			int res, err;
			char *name = Poly_string_to_C_alloc(DEREFSTRINGHANDLE(args));
			res = lstat(name, &buf);
			err = errno;
			free(name);
			if (res < 0) raise_syscall("lstat failed", err);
			return getStatInfo(&buf);
		}

	case 57: /* Get information about an open file. */
		{
			struct stat buf;
			PIOSTRUCT strm = get_stream(args);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (fstat(strm->device.ioDesc, &buf) < 0)
				raise_syscall("fstat failed", errno);
			return getStatInfo(&buf);
		}

	case 58: /* Test access rights to a file. */
		{
			char *name = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			int amode = get_C_long(DEREFHANDLE(args)[1]);
			int res;
			res = access(name, amode);
			free(name);
			/* Return false if error, true if not.  It's not clear that
			   this is correct since there are several reasons why we
			   might get -1 as the result. */
			return Make_arbitrary_precision(res < 0 ? 0 : 1);
		}

	case 59: /* Change access rights. */
		{
			char *name = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			mode_t mode = get_C_long(DEREFHANDLE(args)[1]);
			int err, res;
			res = chmod(name, mode);
			err = errno; /* Save the error result in case free changes it. */
			free(name);
			if (res < 0) raise_syscall("chmod failed", err);
			return Make_arbitrary_precision(0);
		}

	case 60: /* Change access rights on open file. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			mode_t mode = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (fchmod(strm->device.ioDesc, mode) < 0)
				raise_syscall("fchmod failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 61: /* Change owner and group. */
		{
			char *name = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			uid_t uid = get_C_long(DEREFHANDLE(args)[1]);
			gid_t gid = get_C_long(DEREFHANDLE(args)[2]);
			int err, res;
			res = chown(name, uid, gid);
			err = errno; /* Save the error result in case free changes it. */
			free(name);
			if (res < 0) raise_syscall("chown failed", err);
			return Make_arbitrary_precision(0);
		}

	case 62: /* Change owner and group on open file. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			uid_t uid = get_C_long(DEREFHANDLE(args)[1]);
			gid_t gid = get_C_long(DEREFHANDLE(args)[2]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (fchown(strm->device.ioDesc, uid, gid) < 0)
				raise_syscall("fchown failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 63: /* Set access and modification times.  We use utimes rather than utime
		    since it allows us to be more accurate.  There's a similar function
		    in basicio which sets both the access and modification times to the
		    same time. */
		{
			char *name = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			Handle hAccess = (Handle)&DEREFHANDLE(args)[1];
			Handle hMod = (Handle)&DEREFHANDLE(args)[2];
			struct timeval times[2];
			/* We have a value in microseconds.  We need to split
		   	   it into seconds and microseconds.  N.B. The arguments to
		   	   div_longc and rem_longc are in reverse order. */
			Handle hMillion = Make_arbitrary_precision(1000000);
			unsigned secsAccess =
				get_C_ulong(DEREFWORDHANDLE(div_longc(hMillion, hAccess)));
			unsigned usecsAccess =
				get_C_ulong(DEREFWORDHANDLE(rem_longc(hMillion, hAccess)));
			unsigned secsMod =
				get_C_ulong(DEREFWORDHANDLE(div_longc(hMillion, hMod)));
			unsigned usecsMod =
				get_C_ulong(DEREFWORDHANDLE(rem_longc(hMillion, hMod)));
			int err, res;
			times[0].tv_sec = secsAccess;
			times[0].tv_usec = usecsAccess;
			times[1].tv_sec = secsMod;
			times[1].tv_usec = usecsMod;
			res = utimes(name, times);
			err = errno; /* Save the error result in case free changes it. */
			free(name);
			if (res < 0) raise_syscall("utimes failed", err);
			return Make_arbitrary_precision(0);
		}

	case 64: /* Set access and modification times to the current time.  This could be
		    defined in terms of the previous call and Time.now but it could result
		    in an error due to rounding.  This is probably safer. */
		{
			char *name = Poly_string_to_C_alloc(DEREFSTRINGHANDLE(args));
			int err, res;
			res = utimes(name, 0);
			err = errno; /* Save the error result in case free changes it. */
			free(name);
			if (res < 0) raise_syscall("utimes failed", err);
			return Make_arbitrary_precision(0);
		}

	case 65: /* Truncate an open file. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			int size = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (ftruncate(strm->device.ioDesc, size) < 0)
				raise_syscall("ftruncate failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 66: /* Get the configured limits for a file. */
		{
			/* Look up the variable. May raise an exception. */
			int nvar = findPathVar((Handle)&DEREFHANDLE(args)[1]);
			char *name = Poly_string_to_C_alloc((pstring)DEREFHANDLE(args)[0]);
			int err, res;
			/* Set errno to zero.  If there is no limit pathconf returns -1 but
			   does not change errno.  */
			errno = 0;
			res = pathconf(name, nvar);
			err = errno; /* Save the error result in case free changes it. */
			free(name);
			/* We return -1 as a valid result indicating no limit. */
			if (res < 0 && err != 0) raise_syscall("pathconf failed", err);
			return Make_arbitrary_precision(res);
		}

	case 67: /* Get the configured limits for an open file. */
		{
			/* Look up the variable. May raise an exception. */
			int nvar = findPathVar((Handle)&DEREFHANDLE(args)[1]);
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			int res;
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			errno = 0; /* Unchanged if there is no limit. */
			res = fpathconf(strm->device.ioDesc, nvar);
			if (res < 0 && errno != 0) raise_syscall("fpathconf failed", errno);
			return Make_arbitrary_precision(res);
		}

		/* Password and group entries. */
	case 100: /* Get Password entry by name. */
		{
			char pwName[200];
			int length;
			struct passwd *pw;
			length = Poly_string_to_C(DEREFSTRINGHANDLE(args),
						pwName, 200);
			if (length > 200)
				raise_syscall("Password name too long", ENAMETOOLONG);
			pw = getpwnam(pwName);
			if (pw == NULL)
				raise_syscall("Password entry not found", ENOENT);
			return makePasswordEntry(pw);
		}

	case 101: /* Get password entry by uid. */
		{
			int uid = get_C_long(DEREFWORDHANDLE(args));
			struct passwd *pw = getpwuid(uid);
			if (pw == NULL)
				raise_syscall("Password entry not found", ENOENT);
			return makePasswordEntry(pw);
		}

	case 102: /* Get group entry by name. */
		{
			struct group *grp;
			char grpName[200];
			int length;
			length = Poly_string_to_C(DEREFSTRINGHANDLE(args),
						grpName, 200);
			if (length > 200)
				raise_syscall("Group name too long", ENAMETOOLONG);
			grp = getgrnam(grpName);
			if (grp == NULL)
				raise_syscall("Group entry not found", ENOENT);
			return makeGroupEntry(grp);
		}
		
	case 103: /* Get group entry by gid. */
		{
			int gid = get_C_long(DEREFWORDHANDLE(args));
			struct group *grp = getgrgid(gid);
			if (grp == NULL)
				raise_syscall("Group entry not found", ENOENT);
			return makeGroupEntry(grp);
		}

		/* IO Entries. */
	case 110: /* Create a pipe. */
		{
			int filedes[2];
			Handle strRead = make_stream_entry();
			Handle strWrite = make_stream_entry();
			Handle result;
			PIOSTRUCT instrm, outstrm;
			if (pipe(filedes) < 0) raise_syscall("pipe failed", errno);
			instrm = &basic_io_vector[STREAMID(strRead)];
			outstrm = &basic_io_vector[STREAMID(strWrite)];
			instrm->device.ioDesc = filedes[0];
			instrm->ioBits = IO_BIT_OPEN | IO_BIT_READ;
			outstrm->device.ioDesc = filedes[1];
			outstrm->ioBits = IO_BIT_OPEN | IO_BIT_WRITE;
			result = ALLOC(2);
			DEREFHANDLE(result)[0] = DEREFWORDHANDLE(strRead);
			DEREFHANDLE(result)[1] = DEREFWORDHANDLE(strWrite);
			return result;
		}

	case 111: /* Duplicate a file descriptor. */
		{
			PIOSTRUCT str = get_stream(args);
			PIOSTRUCT newstr;
			Handle strToken = make_stream_entry();
			int fd;
			if (str == NULL) raise_syscall("Stream is closed", EBADF);
			fd = dup(str->device.ioDesc);
			if (fd < 0) raise_syscall("dup failed", errno);
			newstr = &basic_io_vector[STREAMID(strToken)];
			newstr->device.ioDesc = fd;
			/* I'm assuming that we're not going to put any other
			   status information in the bits. */
			newstr->ioBits = str->ioBits;
			return strToken;
		}

	case 112: /* Duplicate a file descriptor to a given entry. */
		{
			PIOSTRUCT old = get_stream((Handle)&DEREFHANDLE(args)[0]);
			PIOSTRUCT new = get_stream((Handle)&DEREFHANDLE(args)[1]);
			/* The "new" entry must be an open entry in our io table.
			   It may, though, be an entry added through wordToFD
			   (basicio call 31) which may not refer to a valid
			   descriptor. */
			if (old == NULL || new == NULL)
				raise_syscall("Stream is closed", EBADF);
			if (dup2(old->device.ioDesc, new->device.ioDesc) < 0)
				raise_syscall("dup2 failed", errno);
			new->ioBits = old->ioBits;
			return Make_arbitrary_precision(0);
		}

	case 113: /* Duplicate a file descriptor to an entry equal to or greater
		     than the given value. */
		{
			PIOSTRUCT old = get_stream((Handle)&DEREFHANDLE(args)[0]);
			PIOSTRUCT base = get_stream((Handle)&DEREFHANDLE(args)[1]);
			int newfd;
			Handle strToken = make_stream_entry();
			PIOSTRUCT newstr;
			/* The "base" entry must be an open entry in our io table.
			   It may, though, be an entry added through wordToFD
			   (basicio call 31) which may not refer to a valid
			   descriptor. */
			if (old == NULL || base == NULL)
				raise_syscall("Stream is closed", EBADF);
			newfd = fcntl(old->device.ioDesc, F_DUPFD, base->device.ioDesc);
			if (newfd < 0) raise_syscall("dup2 failed", errno);
			newstr = &basic_io_vector[STREAMID(strToken)];
			newstr->device.ioDesc = newfd;
			/* I'm assuming that we're not going to put any other
			   status information in the bits. */
			newstr->ioBits = old->ioBits;
			return strToken;
		}

	case 114: /* Get the file descriptor flags. */
		{
			PIOSTRUCT strm = get_stream(args);
			int res;
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			res = fcntl(strm->device.ioDesc, F_GETFD);
			if (res < 0) raise_syscall("fcntl failed", errno);
			return Make_arbitrary_precision(res);
		}

	case 115: /* Set the file descriptor flags. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			int flags = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (fcntl(strm->device.ioDesc, F_SETFD, flags) < 0)
				raise_syscall("fcntl failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 116: /* Get the file status and access flags. */
		{
			PIOSTRUCT strm = get_stream(args);
			int res;
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			res = fcntl(strm->device.ioDesc, F_GETFL);
			if (res < 0) raise_syscall("fcntl failed", errno);
			return Make_arbitrary_precision(res);
		}

	case 117: /* Set the file status and access flags. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			int flags = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (fcntl(strm->device.ioDesc, F_SETFL, flags) < 0)
				raise_syscall("fcntl failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 118: /* Seek to a position on the stream. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			long position = get_C_long(DEREFHANDLE(args)[1]);
			int whence = get_C_long(DEREFHANDLE(args)[2]);
			long newpos;
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			newpos = lseek(strm->device.ioDesc, position, whence);
			if (newpos < 0) raise_syscall("lseek failed", errno);
			return Make_arbitrary_precision(newpos);
		}

	case 119: /* Synchronise file contents. */
		{
			PIOSTRUCT strm = get_stream(args);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (fsync(strm->device.ioDesc) < 0) raise_syscall("fsync failed", errno);
			return Make_arbitrary_precision(0);
		}

	case 120: /* get lock */
		return lockCommand(F_GETLK, args);

	case 121: /* set lock */
		return lockCommand(F_SETLK, args);

	case 122: /* wait for lock */
		/* TODO: This may well block the whole process.  We should look at the
		   result and retry if need be. */
		return lockCommand(F_SETLKW, args);

		/* TTY entries. */
	case 150: /* Get attributes. */
		return getTTYattrs(args);

	case 151: /* Set attributes. */
		return setTTYattrs(args);

	case 152: /* Send a break. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			int duration = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (tcsendbreak(strm->device.ioDesc, duration) < 0)
				raise_syscall("tcsendbreak failed", errno);
			return Make_arbitrary_precision(0);
		}
	
	case 153: /* Wait for output to drain. */
		{
			/* TODO: This will block the process.  It really needs to
			   check whether the stream has drained and run another
			   process until it has. */
			PIOSTRUCT strm = get_stream(args);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (tcdrain(strm->device.ioDesc) < 0)
				raise_syscall("tcdrain failed", errno);
			return Make_arbitrary_precision(0);
		}
	
	case 154: /* Flush terminal stream. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			int qs = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (tcflush(strm->device.ioDesc, qs) < 0)
				raise_syscall("tcflush failed", errno);
			return Make_arbitrary_precision(0);
		}
	
	case 155: /* Flow control. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			int action = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (tcflow(strm->device.ioDesc, action) < 0)
				raise_syscall("tcflow failed", errno);
			return Make_arbitrary_precision(0);
		}
	
	case 156: /* Get process group. */
		{
			PIOSTRUCT strm = get_stream(args);
			pid_t pid;
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			pid = tcgetpgrp(strm->device.ioDesc);
			if (pid < 0) raise_syscall("tcgetpgrp failed", errno);
			return Make_arbitrary_precision(pid);
		}
	
	case 157: /* Set process group. */
		{
			PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
			pid_t pid = get_C_long(DEREFHANDLE(args)[1]);
			if (strm == NULL) raise_syscall("Stream is closed", EBADF);
			if (tcsetpgrp(strm->device.ioDesc, pid) < 0)
				raise_syscall("tcsetpgrp failed", errno);
			return Make_arbitrary_precision(0);
		}
	
	default:
		raise_syscall("Unimplemented function", 0);
	}
}

Handle waitForProcess(Handle args)
/* Get result status of a child process. */
{
	int kind = get_C_long(DEREFHANDLE(args)[0]);
	int pid = get_C_long(DEREFHANDLE(args)[1]);
	int callFlags = get_C_long(DEREFHANDLE(args)[2]);
	int flags = callFlags | WNOHANG;
	pid_t pres = 0;
	int status;
	switch (kind)
	{
	case 0: /* Wait for any child. */
		pres = wait3(&status, flags, 0);
		break;
	case 1: /* Wait for specific process. */
		pres = waitpid(pid, &status, flags);
		break;
	case 2: /* Wait for any in current process group. */
#if defined(SOLARIS2)
		pid = getpgrp();
		/* Drop through to next case. */
#else /* FreeBSD and Linux. */
		pres = wait4(0, &status, flags, 0);
		break;
#endif
	case 3: /* Wait for child in given process group */
#if defined(SOLARIS2)
		{
			/* This is a bit messy in Solaris. */ 
			siginfo_t info;
			int res;
			info.si_pid = 0;
			res = waitid(P_GID, pid, &info, flags);
			if (res < 0) raise_syscall("waitid failed", errno);
			pres = info.si_pid;
			/* Encode the status in the same way as wait4. */
			switch (info.si_code)
			{
			case CLD_KILLED: case CLD_DUMPED:
				/* Low order byte is signal */
				status = info.si_status; break;
			case CLD_EXITED:
				/* High order byte is exit status, low order zero. */
				status = info.si_status << 8; break;
			default: /* Stopped */
				/* High order byte is signal, low order 0177. */
				status = (info.si_status << 8) | 0177;
			}
		}
#else /* FreeBSD and Linux. */
		pres = wait4(-pid, &status, flags, 0);
#endif			
	}
	if (pres < 0) raise_syscall("wait failed", errno);
	/* If the caller did not specify WNOHANG but there
	   wasn't a child process waiting we have to block
	  and come back here later. */
	if (pres == 0 && !(callFlags & WNOHANG))
		block_and_restart(-1, 0, POLY_SYS_os_specific);

	/* Construct the result tuple. */
	{
		Handle result, pidHandle, resHandle;
		pidHandle = Make_arbitrary_precision(pres);
		resHandle = Make_arbitrary_precision(status);

		result = ALLOC(2);
		DEREFHANDLE(result)[0] = DEREFWORDHANDLE(pidHandle);
		DEREFHANDLE(result)[1] = DEREFWORDHANDLE(resHandle);
		return result;
	}
}

static Handle makePasswordEntry(struct passwd *pw)
/* Return a password entry. */
{
	Handle nameHandle, uidHandle, gidHandle, homeHandle, shellHandle, result;
	nameHandle = SAVE(C_string_to_Poly(pw->pw_name));
	uidHandle = Make_arbitrary_precision(pw->pw_uid);
	gidHandle = Make_arbitrary_precision(pw->pw_gid);
	homeHandle = SAVE(C_string_to_Poly(pw->pw_dir));
	shellHandle = SAVE(C_string_to_Poly(pw->pw_shell));
	result = ALLOC(5);
	DEREFHANDLE(result)[0] = DEREFWORDHANDLE(nameHandle);
	DEREFHANDLE(result)[1] = DEREFWORDHANDLE(uidHandle);
	DEREFHANDLE(result)[2] = DEREFWORDHANDLE(gidHandle);
	DEREFHANDLE(result)[3] = DEREFWORDHANDLE(homeHandle);
	DEREFHANDLE(result)[4] = DEREFWORDHANDLE(shellHandle);
	return result;
}

static Handle makeGroupEntry(struct group *grp)
{
	Handle nameHandle, gidHandle, membersHandle, result;
	int i;
	char **p;
	nameHandle = SAVE(C_string_to_Poly(grp->gr_name));
	gidHandle = Make_arbitrary_precision(grp->gr_gid);
	/* Group members. */
	for (i=0, p = grp->gr_mem; *p != NULL; p++, i++);
	membersHandle = convert_string_list(i, grp->gr_mem);
	result = ALLOC(3);
	DEREFHANDLE(result)[0] = DEREFWORDHANDLE(nameHandle);
	DEREFHANDLE(result)[1] = DEREFWORDHANDLE(gidHandle);
	DEREFHANDLE(result)[2] = DEREFWORDHANDLE(membersHandle);
	return result;
}

/* Make a cons cell for a pair of strings. */
static void makeStringPairList(Handle list, char *s1, char *s2)
{
	Handle nameHandle, valueHandle, pairHandle, next;
	/* Mark the save stack to prevent overflow. */
	Handle saved = mark_save_vec();
	/* This has to be done carefully to ensure we don't throw anything
	   away if we garbage-collect and also to ensure that each object is
	   fully initialised before the next object is created. */
	/* Make the strings. */
	nameHandle = SAVE(C_string_to_Poly(s1));
	valueHandle = SAVE(C_string_to_Poly(s2));
	/* Make the pair. */
	pairHandle = ALLOC(2);
	DEREFHANDLE(pairHandle)[0] = DEREFWORDHANDLE(nameHandle);
	DEREFHANDLE(pairHandle)[1] = DEREFWORDHANDLE(valueHandle);
	/* Make the cons cell. */
	next  = ALLOC(SIZEOF(ML_Cons_Cell));
	DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(pairHandle); 
	DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
	DEREFHANDLE(list) = DEREFHANDLE(next);
	/* Remove these items from the save stack. */
	reset_save_vec(saved);
}

/* Return the uname information.  */
static Handle getUname(void)
{
	struct utsname name;
	Handle list = SAVE(nil_value);
	if (uname(&name) < 0) raise_syscall("uname failed", errno);
	makeStringPairList(list, "sysname", name.sysname);
	makeStringPairList(list, "nodename", name.nodename);
	makeStringPairList(list, "release", name.release);
	makeStringPairList(list, "version", name.version);
	makeStringPairList(list, "machine", name.machine);
	return list;
}

/* Return the contents of a stat buffer. */
static Handle getStatInfo(struct stat *buf)
{
	int kind;
	Handle result, modeHandle, kindHandle, inoHandle, devHandle, linkHandle;
	Handle uidHandle, gidHandle, sizeHandle, atimeHandle, mtimeHandle, ctimeHandle;
	/* Get the protection mode, masking off the file type info. */
	modeHandle =
		Make_unsigned(buf->st_mode & (S_IRWXU|S_IRWXG|S_IRWXO|S_ISUID|S_ISGID));
	if (S_ISDIR(buf->st_mode)) kind = 1;
	else if (S_ISCHR(buf->st_mode)) kind = 2;
	else if (S_ISBLK(buf->st_mode)) kind = 3;
	else if (S_ISFIFO(buf->st_mode)) kind = 4;
	else if ((buf->st_mode & S_IFMT) == S_IFLNK) kind = 5;
	else if ((buf->st_mode & S_IFMT) == S_IFSOCK) kind = 6;
	else /* Regular. */ kind = 0;
	kindHandle = Make_unsigned(kind);
	inoHandle = Make_unsigned(buf->st_ino);
	devHandle = Make_unsigned(buf->st_dev);
	linkHandle = Make_unsigned(buf->st_nlink);
	uidHandle = Make_unsigned(buf->st_uid);
	gidHandle = Make_unsigned(buf->st_gid);
	sizeHandle = Make_unsigned(buf->st_size);
	/* TODO: The only really standard time fields give the seconds part.  There
	   are various extensions which would give us microseconds or nanoseconds. */
	atimeHandle = Make_arb_from_pair_scaled(buf->st_atime, 0, 1000000);
	mtimeHandle = Make_arb_from_pair_scaled(buf->st_mtime, 0, 1000000);
	ctimeHandle = Make_arb_from_pair_scaled(buf->st_ctime, 0, 1000000);
	result = ALLOC(11);
	DEREFHANDLE(result)[0] = DEREFWORDHANDLE(modeHandle);
	DEREFHANDLE(result)[1] = DEREFWORDHANDLE(kindHandle);
	DEREFHANDLE(result)[2] = DEREFWORDHANDLE(inoHandle);
	DEREFHANDLE(result)[3] = DEREFWORDHANDLE(devHandle);
	DEREFHANDLE(result)[4] = DEREFWORDHANDLE(linkHandle);
	DEREFHANDLE(result)[5] = DEREFWORDHANDLE(uidHandle);
	DEREFHANDLE(result)[6] = DEREFWORDHANDLE(gidHandle);
	DEREFHANDLE(result)[7] = DEREFWORDHANDLE(sizeHandle);
	DEREFHANDLE(result)[8] = DEREFWORDHANDLE(atimeHandle);
	DEREFHANDLE(result)[9] = DEREFWORDHANDLE(mtimeHandle);
	DEREFHANDLE(result)[10] = DEREFWORDHANDLE(ctimeHandle);
	return result;
}



static Handle getTTYattrs(Handle args)
{
	PIOSTRUCT strm = get_stream(args);
	struct termios tios;
	speed_t ispeed, ospeed;
	Handle ifHandle, ofHandle, cfHandle, lfHandle, ccHandle;
	Handle isHandle, osHandle, result;
	if (strm == NULL) raise_syscall("Stream is closed", EBADF);
	if (tcgetattr(strm->device.ioDesc, &tios) < 0)
		raise_syscall("tcgetattr failed", errno);
	/* Extract the speed entries. */
	ospeed = cfgetospeed(&tios);
	ispeed = cfgetispeed(&tios);
	/* Set the speed entries to zero.  In Solaris, at least, the speed is
	   encoded in the flags and we don't want any confusion.  The order of
	   these functions is significant.  */
	cfsetospeed(&tios, B0);
	cfsetispeed(&tios, B0);
	/* Convert the values to ML representation. */
	ifHandle = Make_unsigned(tios.c_iflag);
	ofHandle = Make_unsigned(tios.c_oflag);
	cfHandle = Make_unsigned(tios.c_cflag);
	lfHandle = Make_unsigned(tios.c_lflag);
	/* The cc vector is treated as a string. */
	ccHandle = SAVE(Buffer_to_Poly(tios.c_cc, NCCS));
	isHandle = Make_unsigned(ispeed);
	osHandle = Make_unsigned(ospeed);
	/* We can now create the result tuple. */
	result = ALLOC(7);
	DEREFHANDLE(result)[0] = DEREFWORDHANDLE(ifHandle);
	DEREFHANDLE(result)[1] = DEREFWORDHANDLE(ofHandle);
	DEREFHANDLE(result)[2] = DEREFWORDHANDLE(cfHandle);
	DEREFHANDLE(result)[3] = DEREFWORDHANDLE(lfHandle);
	DEREFHANDLE(result)[4] = DEREFWORDHANDLE(ccHandle);
	DEREFHANDLE(result)[5] = DEREFWORDHANDLE(isHandle);
	DEREFHANDLE(result)[6] = DEREFWORDHANDLE(osHandle);
	return result;
}

/* Assemble the tios structure from the arguments and set the TTY attributes. */
static Handle setTTYattrs(Handle args)
{
	PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
	int actions = get_C_long(DEREFHANDLE(args)[1]);
	struct termios tios;
	speed_t ispeed, ospeed;
	pstring ccv;
	if (strm == NULL) raise_syscall("Stream is closed", EBADF);
	/* Make sure anything unset is zero.  It might be better to call
	   tcgetattr instead. */
	memset(&tios, 0, sizeof(tios));
	tios.c_iflag = get_C_ulong(DEREFHANDLE(args)[2]);
	tios.c_oflag = get_C_ulong(DEREFHANDLE(args)[3]);
	tios.c_cflag = get_C_ulong(DEREFHANDLE(args)[4]);
	tios.c_lflag = get_C_ulong(DEREFHANDLE(args)[5]);
	/* The cc vector should be a string of exactly NCCS characters.  It
	   may well contain nulls so we can't use Poly_string_to_C to copy it. */
	ccv = (pstring)DEREFHANDLE(args)[6];
	if (IS_INT(ccv) || ccv->length != NCCS) /* Just to check. */
		raise_syscall("Incorrect cc vector", EINVAL);
	memcpy(tios.c_cc, ccv->chars, NCCS);
	ispeed = get_C_ulong(DEREFHANDLE(args)[7]);
	ospeed = get_C_ulong(DEREFHANDLE(args)[8]);
	if (cfsetispeed(&tios, ispeed) < 0)
		raise_syscall("cfsetispeed failed", errno);
	if (cfsetospeed(&tios, ospeed) < 0)
		raise_syscall("cfsetospeed failed", errno);
	/* Now it's all set we can call tcsetattr to do the work. */
	if (tcsetattr(strm->device.ioDesc, actions, &tios) < 0)
		raise_syscall("tcsetattr failed", errno);
	return Make_arbitrary_precision(0);
}

/* Lock/unlock/test file locks.  Returns the, possibly modified, argument structure. */
static Handle lockCommand(int cmd, Handle args)
{
	PIOSTRUCT strm = get_stream((Handle)&DEREFHANDLE(args)[0]);
	struct flock lock;
	Handle result, typeHandle, whenceHandle, startHandle, lenHandle, pidHandle;
	memset(&lock, 0, sizeof(lock)); /* Make sure unused fields are zero. */
	if (strm == NULL) raise_syscall("Stream is closed", EBADF);
	lock.l_type = get_C_long(DEREFHANDLE(args)[1]);
	lock.l_whence = get_C_long(DEREFHANDLE(args)[2]);
	lock.l_start = get_C_long(DEREFHANDLE(args)[3]);
	lock.l_len = get_C_long(DEREFHANDLE(args)[4]);
	lock.l_pid = get_C_long(DEREFHANDLE(args)[5]);
	if (fcntl(strm->device.ioDesc, cmd, &lock) < 0) 
		raise_syscall("fcntl failed", errno);
	/* Construct the result. */
	typeHandle = Make_arbitrary_precision(lock.l_type);
	whenceHandle = Make_arbitrary_precision(lock.l_whence);
	startHandle = Make_arbitrary_precision(lock.l_start);
	lenHandle = Make_arbitrary_precision(lock.l_len);
	pidHandle = Make_arbitrary_precision(lock.l_pid);
	result = ALLOC(5);
	DEREFHANDLE(result)[0] = DEREFWORDHANDLE(typeHandle);
	DEREFHANDLE(result)[1] = DEREFWORDHANDLE(whenceHandle);
	DEREFHANDLE(result)[2] = DEREFWORDHANDLE(startHandle);
	DEREFHANDLE(result)[3] = DEREFWORDHANDLE(lenHandle);
	DEREFHANDLE(result)[4] = DEREFWORDHANDLE(pidHandle);
	return result;
}


/* This table maps string arguments for sysconf into the corresponding constants. */
/* These are highly OS dependent.  It has been configured on Solaris 2.8, Linux Redhat 5.2
   and FreeBSD 3.4. */
static struct {
	char *saName;
	int saVal;
} sysArgTable[] =
{
	{ "_SC_ARG_MAX",		_SC_ARG_MAX },
	{ "_SC_CHILD_MAX",		_SC_CHILD_MAX },
	{ "_SC_CLK_TCK",		_SC_CLK_TCK },
	{ "_SC_NGROUPS_MAX",		_SC_NGROUPS_MAX },
	{ "_SC_OPEN_MAX",		_SC_OPEN_MAX },
	{ "_SC_JOB_CONTROL",		_SC_JOB_CONTROL },
	{ "_SC_SAVED_IDS",		_SC_SAVED_IDS },
	{ "_SC_VERSION",		_SC_VERSION },
#ifdef _SC_PASS_MAX
	{ "_SC_PASS_MAX",		_SC_PASS_MAX },
#endif
#ifdef _SC_LOGNAME_MAX
	{ "_SC_LOGNAME_MAX",		_SC_LOGNAME_MAX },
#endif
#ifdef _SC_PAGESIZE
	{ "_SC_PAGESIZE",		_SC_PAGESIZE },
#endif
#ifdef _SC_XOPEN_VERSION
	{ "_SC_XOPEN_VERSION",		_SC_XOPEN_VERSION },
#endif
#ifdef SC_NPROCESSORS_CONF
	{ "_SC_NPROCESSORS_CONF",	_SC_NPROCESSORS_CONF },
#endif
#ifdef _SC_NPROCESSORS_ONLN
	{ "_SC_NPROCESSORS_ONLN",	_SC_NPROCESSORS_ONLN },
#endif
	{ "_SC_STREAM_MAX",		_SC_STREAM_MAX },
	{ "_SC_TZNAME_MAX",		_SC_TZNAME_MAX },
#ifdef _SC_AIO_LISTIO_MAX
	{ "_SC_AIO_LISTIO_MAX",		_SC_AIO_LISTIO_MAX },
#endif
#ifdef _SC_AIO_MAX
	{ "_SC_AIO_MAX",		_SC_AIO_MAX },
#endif
#ifdef _SC_AIO_PRIO_DELTA_MAX
	{ "_SC_AIO_PRIO_DELTA_MAX",	_SC_AIO_PRIO_DELTA_MAX },
#endif
#ifdef _SC_ASYNCHRONOUS_IO
	{ "_SC_ASYNCHRONOUS_IO",	_SC_ASYNCHRONOUS_IO },
#endif
#ifdef _SC_DELAYTIMER_MAX
	{ "_SC_DELAYTIMER_MAX",		_SC_DELAYTIMER_MAX },
#endif
#ifdef _SC_FSYNC
	{ "_SC_FSYNC",			_SC_FSYNC },
#endif
#ifdef _SC_MAPPED_FILES
	{ "_SC_MAPPED_FILES",		_SC_MAPPED_FILES },
#endif
#ifdef _SC_MEMLOCK
	{ "_SC_MEMLOCK",		_SC_MEMLOCK },
#endif
#ifdef _SC_MEMLOCK_RANGE
	{ "_SC_MEMLOCK_RANGE",		_SC_MEMLOCK_RANGE },
#endif
#ifdef _SC_MEMORY_PROTECTION
	{ "_SC_MEMORY_PROTECTION",	_SC_MEMORY_PROTECTION },
#endif
#ifdef _SC_MESSAGE_PASSING
	{ "_SC_MESSAGE_PASSING",	_SC_MESSAGE_PASSING },
#endif
#ifdef _SC_MQ_OPEN_MAX
	{ "_SC_MQ_OPEN_MAX",		_SC_MQ_OPEN_MAX },
#endif
#ifdef _SC_MQ_PRIO_MAX
	{ "_SC_MQ_PRIO_MAX",		_SC_MQ_PRIO_MAX },
#endif
#if (! (defined(MACOSX) || (defined(FREEBSD) && __FreeBSD__ <= 2)))
	{ "_SC_PRIORITIZED_IO",		_SC_PRIORITIZED_IO },
	{ "_SC_PRIORITY_SCHEDULING",	_SC_PRIORITY_SCHEDULING },
	{ "_SC_REALTIME_SIGNALS",	_SC_REALTIME_SIGNALS },
	{ "_SC_RTSIG_MAX",		_SC_RTSIG_MAX },
	{ "_SC_SEMAPHORES",		_SC_SEMAPHORES },
	{ "_SC_SEM_NSEMS_MAX",		_SC_SEM_NSEMS_MAX },
	{ "_SC_SEM_VALUE_MAX",		_SC_SEM_VALUE_MAX },
	{ "_SC_SHARED_MEMORY_OBJECTS",	_SC_SHARED_MEMORY_OBJECTS },
	{ "_SC_SIGQUEUE_MAX",		_SC_SIGQUEUE_MAX },
#endif
#ifdef SOLARIS2
	{ "_SC_SIGRT_MIN",		_SC_SIGRT_MIN },
	{ "_SC_SIGRT_MAX",		_SC_SIGRT_MAX },
#endif
#ifdef _SC_SYNCHRONIZED_IO
	{ "_SC_SYNCHRONIZED_IO",	_SC_SYNCHRONIZED_IO },
#endif
#ifdef _SC_TIMERS
	{ "_SC_TIMERS",			_SC_TIMERS },
#endif
#ifdef _SC_TIMER_MAX
	{ "_SC_TIMER_MAX",		_SC_TIMER_MAX },
#endif
#ifdef _SC_2_C_BIND
	{ "_SC_2_C_BIND",		_SC_2_C_BIND },
#endif
#ifdef _SC_2_C_DEV
	{ "_SC_2_C_DEV",		_SC_2_C_DEV },
#endif
#ifdef _SC_2_C_VERSION
	{ "_SC_2_C_VERSION",		_SC_2_C_VERSION },
#endif
	{ "_SC_2_FORT_DEV",		_SC_2_FORT_DEV },
	{ "_SC_2_FORT_RUN",		_SC_2_FORT_RUN },
	{ "_SC_2_LOCALEDEF",		_SC_2_LOCALEDEF },
	{ "_SC_2_SW_DEV",		_SC_2_SW_DEV },
	{ "_SC_2_UPE",			_SC_2_UPE },
	{ "_SC_2_VERSION",		_SC_2_VERSION },
	{ "_SC_BC_BASE_MAX",		_SC_BC_BASE_MAX },
	{ "_SC_BC_DIM_MAX",		_SC_BC_DIM_MAX },
	{ "_SC_BC_SCALE_MAX",		_SC_BC_SCALE_MAX },
	{ "_SC_BC_STRING_MAX",		_SC_BC_STRING_MAX },
	{ "_SC_COLL_WEIGHTS_MAX",	_SC_COLL_WEIGHTS_MAX },
	{ "_SC_EXPR_NEST_MAX",		_SC_EXPR_NEST_MAX },
	{ "_SC_LINE_MAX",		_SC_LINE_MAX },
	{ "_SC_RE_DUP_MAX",		_SC_RE_DUP_MAX },
#ifdef _SC_XOPEN_CRYPT
	{ "_SC_XOPEN_CRYPT",		_SC_XOPEN_CRYPT },
#endif
#ifdef _SC_XOPEN_ENH_I18N
	{ "_SC_XOPEN_ENH_I18N",		_SC_XOPEN_ENH_I18N },
#endif
#ifdef _SC_XOPEN_SHM
	{ "_SC_XOPEN_SHM",		_SC_XOPEN_SHM },
#endif
	{ "_SC_2_CHAR_TERM",		_SC_2_CHAR_TERM },
#ifdef _SC_XOPEN_XCU_VERSION
	{ "_SC_XOPEN_XCU_VERSION",	_SC_XOPEN_XCU_VERSION },
#endif
#ifdef _SC_ATEXIT_MAX
	{ "_SC_ATEXIT_MAX",		_SC_ATEXIT_MAX },
#endif
#ifdef _SC_IOV_MAX
	{ "_SC_IOV_MAX",		_SC_IOV_MAX },
#endif
#ifdef _SC_XOPEN_UNIX
	{ "_SC_XOPEN_UNIX",		_SC_XOPEN_UNIX },
#endif
#ifdef _SC_PAGE_SIZE
	{ "_SC_PAGE_SIZE",		_SC_PAGE_SIZE },
#endif
#ifdef _SC_T_IOV_MAX
	{ "_SC_T_IOV_MAX",		_SC_T_IOV_MAX },
#endif
#ifdef _SC_PHYS_PAGES
	{ "_SC_PHYS_PAGES",		_SC_PHYS_PAGES },
#endif
#ifdef _SC_AVPHYS_PAGES
	{ "_SC_AVPHYS_PAGES",		_SC_AVPHYS_PAGES },
#endif
#ifdef _SC_COHER_BLKSZ
	{ "_SC_COHER_BLKSZ",		_SC_COHER_BLKSZ },
#endif
#ifdef _SC_SPLIT_CACHE
	{ "_SC_SPLIT_CACHE",		_SC_SPLIT_CACHE },
#endif
#ifdef _SC_ICACHE_SZ
	{ "_SC_ICACHE_SZ",		_SC_ICACHE_SZ },
#endif
#ifdef _SC_DCACHE_SZ
	{ "_SC_DCACHE_SZ",		_SC_DCACHE_SZ },
#endif
#ifdef _SC_ICACHE_LINESZ
	{ "_SC_ICACHE_LINESZ",		_SC_ICACHE_LINESZ },
#endif
#ifdef _SC_DCACHE_LINESZ
	{ "_SC_DCACHE_LINESZ",		_SC_DCACHE_LINESZ },
#endif
#ifdef _SC_ICACHE_BLKSZ
	{ "_SC_ICACHE_BLKSZ",		_SC_ICACHE_BLKSZ },
#endif
#ifdef _SC_DCACHE_BLKSZ
	{ "_SC_DCACHE_BLKSZ",		_SC_DCACHE_BLKSZ },
#endif
#ifdef _SC_DCACHE_TBLKSZ
	{ "_SC_DCACHE_TBLKSZ",		_SC_DCACHE_TBLKSZ },
#endif
#ifdef _SC_ICACHE_ASSOC
	{ "_SC_ICACHE_ASSOC",		_SC_ICACHE_ASSOC },
#endif
#ifdef _SC_DCACHE_ASSOC
	{ "_SC_DCACHE_ASSOC",		_SC_DCACHE_ASSOC },
#endif
#ifdef _SC_MAXPID
	{ "_SC_MAXPID",			_SC_MAXPID },
#endif
#ifdef _SC_STACK_PROT
	{ "_SC_STACK_PROT",		_SC_STACK_PROT },
#endif
#ifdef _SC_THREAD_DESTRUCTOR_ITERATIONS
	{ "_SC_THREAD_DESTRUCTOR_ITERATIONS",	_SC_THREAD_DESTRUCTOR_ITERATIONS },
#endif
#ifdef _SC_GETGR_R_SIZE_MAX
	{ "_SC_GETGR_R_SIZE_MAX",	_SC_GETGR_R_SIZE_MAX },
#endif
#ifdef _SC_GETPW_R_SIZE_MAX
	{ "_SC_GETPW_R_SIZE_MAX",	_SC_GETPW_R_SIZE_MAX },
#endif
#ifdef _SC_LOGIN_NAME_MAX
	{ "_SC_LOGIN_NAME_MAX",		_SC_LOGIN_NAME_MAX },
#endif
#ifdef _SC_THREAD_KEYS_MAX
	{ "_SC_THREAD_KEYS_MAX",	_SC_THREAD_KEYS_MAX },
#endif
#ifdef _SC_THREAD_STACK_MI
	{ "_SC_THREAD_STACK_MIN",	_SC_THREAD_STACK_MIN },
#endif
#ifdef _SC_THREAD_THREADS_MAX
	{ "_SC_THREAD_THREADS_MAX",	_SC_THREAD_THREADS_MAX },
#endif
#ifdef _SC_THREAD_ATTR_STACKADDR
	{ "_SC_THREAD_ATTR_STACKADDR",	_SC_THREAD_ATTR_STACKADDR },
#endif
#ifdef _SC_THREAD_ATTR_STACKSIZE
	{ "_SC_THREAD_ATTR_STACKSIZE",	_SC_THREAD_ATTR_STACKSIZE },
#endif
#ifdef _SC_THREAD_PRIORITY_SCHEDULING
	{ "_SC_THREAD_PRIORITY_SCHEDULING",	_SC_THREAD_PRIORITY_SCHEDULING },
#endif
#ifdef _SC_THREAD_PRIO_INHERIT
	{ "_SC_THREAD_PRIO_INHERIT",	_SC_THREAD_PRIO_INHERIT },
#endif
#ifdef _SC_THREAD_PRIO_PROTECT
	{ "_SC_THREAD_PRIO_PROTECT",	_SC_THREAD_PRIO_PROTECT },
#endif
#ifdef _SC_THREAD_PROCESS_SHARED
	{ "_SC_THREAD_PROCESS_SHARED",	_SC_THREAD_PROCESS_SHARED },
#endif
#ifdef _SC_XOPEN_LEGACY
	{ "_SC_XOPEN_LEGACY",		_SC_XOPEN_LEGACY },
#endif
#ifdef _SC_XOPEN_REALTIME
	{ "_SC_XOPEN_REALTIME",		_SC_XOPEN_REALTIME },
#endif
#ifdef _SC_XOPEN_REALTIME_THREADS
	{ "_SC_XOPEN_REALTIME_THREADS",	_SC_XOPEN_REALTIME_THREADS },
#endif
#ifdef _SC_XBS5_ILP32_OFF32
	{ "_SC_XBS5_ILP32_OFF32",	_SC_XBS5_ILP32_OFF32 },
#endif
#ifdef _SC_XBS5_ILP32_OFFBIG
	{ "_SC_XBS5_ILP32_OFFBIG",	_SC_XBS5_ILP32_OFFBIG },
#endif
#ifdef _SC_XBS5_LP64_OFF64
	{ "_SC_XBS5_LP64_OFF64",	_SC_XBS5_LP64_OFF64 },
#endif
#ifdef _SC_XBS5_LPBIG_OFFBIG
	{ "_SC_XBS5_LPBIG_OFFBIG",	_SC_XBS5_LPBIG_OFFBIG },
#endif
#ifdef LINUX
	{ "_SC_EQUIV_CLASS_MAX",	_SC_EQUIV_CLASS_MAX },
	{ "_SC_CHARCLASS_NAME_MAX",	_SC_CHARCLASS_NAME_MAX },
	{ "_SC_PII",			_SC_PII },
	{ "_SC_PII_XTI",		_SC_PII_XTI },
	{ "_SC_PII_SOCKET",		_SC_PII_SOCKET },
	{ "_SC_PII_INTERNET",		_SC_PII_INTERNET },
	{ "_SC_PII_OSI",		_SC_PII_OSI },
	{ "_SC_POLL",			_SC_POLL },
	{ "_SC_SELECT",			_SC_SELECT },
	{ "_SC_UIO_MAXIOV",		_SC_UIO_MAXIOV },
	{ "_SC_PII_INTERNET_STREAM",	_SC_PII_INTERNET_STREAM },
	{ "_SC_PII_INTERNET_DGRAM",	_SC_PII_INTERNET_DGRAM },
	{ "_SC_PII_OSI_COTS",		_SC_PII_OSI_COTS },
	{ "_SC_PII_OSI_CLTS",		_SC_PII_OSI_CLTS },
	{ "_SC_PII_OSI_M",		_SC_PII_OSI_M },
	{ "_SC_T_IOV_MAX",		_SC_T_IOV_MAX },
	{ "_SC_THREADS",		_SC_THREADS },
	{ "_SC_THREAD_SAFE_FUNCTIONS",	_SC_THREAD_SAFE_FUNCTIONS },
	{ "_SC_TTY_NAME_MAX",		_SC_TTY_NAME_MAX },
	{ "_SC_XOPEN_XPG2",		_SC_XOPEN_XPG2 },
	{ "_SC_XOPEN_XPG3",		_SC_XOPEN_XPG3 },
	{ "_SC_XOPEN_XPG4",		_SC_XOPEN_XPG4 },
	{ "_SC_CHAR_BIT",		_SC_CHAR_BIT },
	{ "_SC_CHAR_MAX",		_SC_CHAR_MAX },
	{ "_SC_CHAR_MIN",		_SC_CHAR_MIN },
	{ "_SC_INT_MAX",		_SC_INT_MAX },
	{ "_SC_INT_MIN",		_SC_INT_MIN },
	{ "_SC_LONG_BIT",		_SC_LONG_BIT },
	{ "_SC_WORD_BIT",		_SC_WORD_BIT },
	{ "_SC_MB_LEN_MAX",		_SC_MB_LEN_MAX },
	{ "_SC_NZERO",			_SC_NZERO },
	{ "_SC_SSIZE_MAX",		_SC_SSIZE_MAX },
	{ "_SC_SCHAR_MAX",		_SC_SCHAR_MAX },
	{ "_SC_SCHAR_MIN",		_SC_SCHAR_MIN },
	{ "_SC_SHRT_MAX",		_SC_SHRT_MAX },
	{ "_SC_SHRT_MIN",		_SC_SHRT_MIN },
	{ "_SC_UCHAR_MAX",		_SC_UCHAR_MAX },
	{ "_SC_UINT_MAX",		_SC_UINT_MAX },
	{ "_SC_ULONG_MAX",		_SC_ULONG_MAX },
	{ "_SC_USHRT_MAX",		_SC_USHRT_MAX },
	{ "_SC_NL_ARGMAX",		_SC_NL_ARGMAX },
	{ "_SC_NL_LANGMAX",		_SC_NL_LANGMAX },
	{ "_SC_NL_MSGMAX",		_SC_NL_MSGMAX },
	{ "_SC_NL_NMAX",		_SC_NL_NMAX },
	{ "_SC_NL_SETMAX",		_SC_NL_SETMAX },
#endif
};

static Handle getSysConf(Handle args)
{
	char argName[200];
	int length, i;
	long res;
	length = Poly_string_to_C(DEREFSTRINGHANDLE(args), argName, 200);
	if (length > 200) raise_syscall("Argument name too long", ENAMETOOLONG);

	for (i = 0; i < sizeof(sysArgTable)/sizeof(sysArgTable[0]); i++) {
		if (strcmp(argName, sysArgTable[i].saName) == 0) break;
		/* See if it matches without the _SC_ at the beginning. */
		if (strcmp(argName, sysArgTable[i].saName+4) == 0) break;
	}
	if (i == sizeof(sysArgTable)/sizeof(sysArgTable[0]))
		raise_syscall("sysconf argument not found", EINVAL);
	errno = 0; /* Sysconf may return -1 without updating errno. */
	res = sysconf(sysArgTable[i].saVal);
	if (res < 0) raise_syscall("sysconf failed", errno);
	return Make_arbitrary_precision(res);
}


static struct {
	char *pcName;
	int pcVal;
} pathConfTable[] =
{
	{ "_PC_LINK_MAX",	_PC_LINK_MAX },
	{ "_PC_MAX_CANON", 	_PC_MAX_CANON },
	{ "_PC_MAX_INPUT", 	_PC_MAX_INPUT },
	{ "_PC_NAME_MAX", 	_PC_NAME_MAX },
	{ "_PC_PATH_MAX", 	_PC_PATH_MAX },
	{ "_PC_PIPE_BUF", 	_PC_PIPE_BUF },
	{ "_PC_NO_TRUNC", 	_PC_NO_TRUNC },
	{ "_PC_VDISABLE", 	_PC_VDISABLE },
	{ "_PC_CHOWN_RESTRICTED", 	_PC_CHOWN_RESTRICTED },
#ifdef _PC_ASYNC_IO
	{ "_PC_ASYNC_IO", 	_PC_ASYNC_IO },
#endif
#ifdef _PC_PRIO_IO
	{ "_PC_PRIO_IO", 	_PC_PRIO_IO },
#endif
#ifdef _PC_SYNC_IO
	{ "_PC_SYNC_IO", 	_PC_SYNC_IO },
#endif
#ifdef _PC_FILESIZEBITS
	{ "_PC_FILESIZEBITS", 	_PC_FILESIZEBITS },
#endif
#ifdef _PC_SOCK_MAXBUF
	{ "_PC_SOCK_MAXBUF", 	_PC_SOCK_MAXBUF },
#endif
};

/* Look up a path variable in the table. */
static int findPathVar(Handle hString)
{
	char argName[200];
	int length, i;
	length = Poly_string_to_C(DEREFSTRINGHANDLE(hString), argName, 200);
	if (length > 200) raise_syscall("Argument name too long", ENAMETOOLONG);

	for (i = 0; i < sizeof(pathConfTable)/sizeof(pathConfTable[0]); i++) {
		if (strcmp(argName, pathConfTable[i].pcName) == 0)
			return pathConfTable[i].pcVal;
		/* See if it matches without the _PC_ at the beginning. */
		if (strcmp(argName, pathConfTable[i].pcName+4) == 0)
			return pathConfTable[i].pcVal;
	}
	raise_syscall("pathconf argument not found", EINVAL);
}

void uninit_os_specific(void)
{
}

static void UnixInterrupts(int signum /* not used */)
{
	/* Check the alarm clock. */
	if (alarmclock.tv_sec != 0 || alarmclock.tv_usec != 0)
	{
		struct timeval tv;
		struct timezone tz;
		if (gettimeofday(&tv, &tz) != 0) return;
		/* If the current time is after the alarm clock we schedule
		   an interrupt and clear the timer. */
		if (tv.tv_sec > alarmclock.tv_sec ||
		   (tv.tv_sec == alarmclock.tv_sec && tv.tv_usec >= alarmclock.tv_usec))
		{
			alarmclock.tv_sec = alarmclock.tv_usec = 0;
			/* Simulate a SIGALRM. */
			addSigCount(SIGALRM);
		}
	}
}

void init_os_specific(void)
{
	struct sigaction sigcatch;
	register_interrupt_proc(UnixInterrupts);

	/* Ignore SIGPIPE - return any errors as failure to write. */
	memset(&sigcatch, 0, sizeof(sigcatch));
	sigcatch.sa_handler = SIG_IGN;
	sigaction(SIGPIPE, &sigcatch, NULL);
}

void re_init_os_specific(void)
{
}

