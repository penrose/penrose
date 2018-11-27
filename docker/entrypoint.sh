#!/bin/bash

usage () {

    echo "Usage:

         docker run <container> [help|web]
         docker run -p 8000:8000 -p 9060:9060 <container> web

         Options:
            --port -p :    change the penrose port (default 8000)
            --template -t: choose a penrose template to run (blank to see options)
            --custom:  -c: provide your own determinants.sub, *.sty, and *.dsl
                           as input to the penrose executable

         Commands:

                help: show help and exit
                web: start penrose frontend (requires mapping of ports)         

         Examples:
             docker run -p 8000:8000 -p 9060:9060 <container> web --custom math.sub math.sty math.det
             docker run -p 8000:8000 -p 9060:9060 <container> web linear-algebra


         "
}

PENROSE_WEB="no"
PENROSE_PORT="8000"
PENROSE_TEMPLATE=""
PENROSE_CUSTOM="no"

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
        --custom|c)
            shift
            PENROSE_CUSTOM="yes"
        ;;
        --template|-t)
            shift
            PENROSE_TEMPLATE="${1:-}"
            shift
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

if [ "${PENROSE_WEB}" == "yes" ]; then

    # For web, the user MUST have custom string, OR template

    if [ "${PENROSE_CUSTOM}" == "yes" ]; then

        PENROSE_COMMAND="$@"

    elif [ "${PENROSE_TEMPLATE}" != "" ]; then

        PENROSE_STYLE="/penrose/src/domains/${PENROSE_TEMPLATE}/${PENROSE_TEMPLATE}.sty"
        PENROSE_DSL="/penrose/src/domains/${PENROSE_TEMPLATE}/${PENROSE_TEMPLATE}.dsl"
        PENROSE_DET="/penrose/src/domains/${PENROSE_TEMPLATE}/determinants.sub"
        PENROSE_COMMAND="${PENROSE_DET} ${PENROSE_STYLE} ${PENROSE_DSL} $@"

    else
        echo "Please specify a template to run! Choices are:"
        echo
        ls -1 /penrose/src/domains
        exit
    fi
   
    echo "Starting Development Web Server"
    echo "Open your browser to localhost:${PENROSE_PORT}/client.html"
    echo
    cd /penrose/src/front-end && 
       (python3 -m http.server ${PENROSE_PORT} &)
        penrose ${PENROSE_COMMAND}
    #(cd /penrose/src/front-end && mini_httpd -p 8000 &)
    #exec penrose ${PENROSE_COMMAND}

else
    exec /root/.local/bin/penrose "$@"
fi
