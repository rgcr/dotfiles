pmrs() { PORT=${1:-8080}; python manage.py runserver 0.0.0.0:${PORT}   }
pmrsp() { PORT=${1:-8080}; python manage.py runserver_plus 0.0.0.0:${PORT} }

alias pm='python manage.py'
alias pmmm='python manage.py makemigrations'
alias pms='python manage.py syncdb'
alias pmm='python manage.py migrate'
alias pmsh='python manage.py shell'
alias pmshp='python manage.py shell_plus'
