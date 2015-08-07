pmrs() { python manage.py runserver 0.0.0.0:$1   }
pmrsp() { python manage.py runserver_plus 0.0.0.0:$1   }

alias pms='python manage.py syncdb'
alias pmm='python manage.py migrate'
alias pmsh='python manage.py shell'
alias pmshp='python manage.py shell_plus'
