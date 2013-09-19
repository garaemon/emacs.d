#!/bin/sh


###########################################################
# originaly from check_for_upgrade.sh and upgrade.sh
# of oh-my-zsh
###########################################################

function _current_epoch() {
  echo $(($(date +%s) / 60 / 60 / 24))
}

function _update_emacs_update() {
  echo "LAST_EPOCH=$(_current_epoch)" > ~/.emacs-update
}

function _upgrade_emacs() {
  (cd ~/.emacs.d && git pull --rebase origin master)
  # update the emacs file
  _update_emacs_update
}

epoch_target=$UPDATE_EMACS_DAYS
if [[ -z "$epoch_target" ]]; then
  # Default to old behavior, default 2 weeks
  epoch_target=13
fi

if [ -f ~/.emacs-update ]; then
    . ~/.emacs-update
    if [[ -z "$LAST_EPOCH" ]]; then # something wrong (malformed .emacs-update)
        _update_emacs_update && return 0;
    fi
    epoch_diff=$(($(_current_epoch) - $LAST_EPOCH))
    if [ $epoch_diff -gt $epoch_target ]; then
        echo "[emacs.d] Would you like to check for updates?"
        echo "Type Y to update emacs.d: \c"
        read line
        if [ "$line" = Y ] || [ "$line" = y ]; then
            _upgrade_emacs
        else
            _update_emacs_update
        fi
    fi
else                            # create ~/.emacs-update
    _update_emacs_update
fi

