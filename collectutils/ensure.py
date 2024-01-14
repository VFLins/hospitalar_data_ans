import os
import ftplib
from prefs import Conf
from pathlib import PureWindowsPath

def normal_path(
        path:       str, 
        as_posix:   bool=True
    ) -> str:
    
    """
    Normalizes a path received.

    Args:

    - path (`str`): The path to be normalized;
    - as_posix (`bool`): If `True`, output will be in posix style, otherwise returns in current OS style (might still be posix). Defaults to `True`.

    Returns:

    A string with path normalized

    """

    if os.path.sep == '\\':
        requested_path = PureWindowsPath(path).as_posix()
    else: 
        requested_path = os.path.normalpath()
    
    return

def change_remote_wd(
        ftp:        ftplib.FPT, 
        path:       str,
        ) ->        bool:
    
    """
    Tries to change directory on remote `ftp` server to `path` and returns a boolean signal indicating operation success.

    Args:

    - ftp (`ftplib.FPT`): An instance of the FTP class, expects to be already connected;
    - path (`str`): FULL remote path targeted;

    Returns:

    `bool` indicating wether the operation was successful or not.
    """

    path = normal_path(path)
    
    for attempt in range(1, Conf.retry+1):
        try:

            if Conf.verbose:
                print(f"connecting to {path}")

            ftp.cwd(path)
            if Conf.verbose:
                print(
                    f"Request completed after {attempt} attempts!"
                    f"Now we are in {path}",
                    sep="\n")
            break

        except ftplib.error_reply as reply:
            if Conf.verbose:
                print(
                    f"Server respnded: {reply}",
                    "Waiting...", sep="\n")

        except Exception:
            print("Unexpected error:\n", Exception)

    current_path = normal_path(ftp.pwd())

    return current_path == path