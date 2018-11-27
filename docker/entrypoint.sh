#!/bin/bash

usage () {

    echo "Usage:

         docker run <container> [help|web]
         docker run -p 8000:8000 -p 9060:9060 <container> web

         Options:
            --port -p :    change the penrose port (default 8000)
            --template -t: choose a penrose template to run (blank to see options)

         Commands:

                help: show help and exit
                web: start penrose frontend (requires mapping of ports)         
         "
}

PENROSE_WEB="no"
PENROSE_PORT="8000"
PENROSE_TEMPLATE=""

while true; do
    case ${1:-} in
        -h|--help|help)
            usage
            exit
        ;;
        --web|web)
            shift
            PENROSE_WEB="yes"
        ;;
        --template|t)
            shift
            PENROSE_WEB="yes"
        ;;
        --port|-p)
            shift
            PENROSE_PORT="${1:-}"
            shift
        ;;
        -*)
            echo "Unknown option: ${1:-}"
            exit 1
        ;;
        *)
            break
        ;;
    esac
done

# The user must supply a template name

if [ "${PENROSE_TEMPLATE}" == "" ]; then
    echo "Please specify a template to run! Choices are:"
    echo
    ls -1 /penrose/src/domains
    exit
fi

# Are we starting the web server?

if [ "${PENROSE_WEB}" == "yes" ]; then

    echo "Starting Development Web Server"
    echo "Open your browser to 127.0.0.1:${PENROSE_PORT}"
    echo
    cd /penrose/src/front-end && 
       (python3 -m http.server ${PENROSE_PORT}) &&
        penrose "$@"

else
    exec /root/.local/bin/penrose "$@"
fi   
