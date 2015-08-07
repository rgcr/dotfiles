setproxy() 
{
    p=${1}; np=${2:-localhost}
    no_proxy=$np; NO_PROXY=$np
    http_proxy=$p; HTTP_PROXY=$p; https_proxy=$p; HTTPS_PROXY=$p; ftp_proxy=$p; FTP_PROXY=$p
    export http_proxy https_proxy HTTP_PROXY HTTPS_PROXY ftp_proxy FTP_PROXY no_proxy NO_PROXY
}

noproxy() 
{
    unset http_proxy https_proxy ftp_proxy no_proxy HTTP_PROXY HTTPS_PROXY FTP_PROXY NO_PROXY
    echo "unset proxy!"
}


alias unsetproxy='noproxy'
