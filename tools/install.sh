#!/bin/sh

## Nova installation script

REPO=${REPO:-novaframework/nova}
REMOTE=${REMOTE:-https://github.com/${REPO}.git}
BRANCH=${BRANCH:-templates}
TEMPLATE_DIR=${TEMPLATE_DIR:-~/.config/rebar3/templates}

command_exists() {
	command -v "$@" >/dev/null 2>&1
}

error() {
	echo ${RED}"Error: $@"${RESET} >&2
}


setup_color() {
	# Only use colors if connected to a terminal
	if [ -t 1 ]; then
		RED=$(printf '\033[31m')
		GREEN=$(printf '\033[32m')
		YELLOW=$(printf '\033[33m')
		BLUE=$(printf '\033[34m')
		BOLD=$(printf '\033[1m')
		RESET=$(printf '\033[m')
	else
		RED=""
		GREEN=""
		YELLOW=""
		BLUE=""
		BOLD=""
		RESET=""
	fi
}

setup_templates() {
    tmp_dir=$(mktemp -d -t nova-XXXXXX)
    git clone --depth=1 --branch "$BRANCH" "$REMOTE" "$tmp_dir" || {
	error "git clone failed."
	rm -rf $tmp_dir
	exit 1
    }

    if ! command_exists rebar3; then
	printf "$YELLOW"
	cat <<-'EOF'
	    rebar3 could not be found in your PATH. This might be cause you are
	    running a local installation or forgot to install it. If you don't
	    have rebar3 installed please refer to their homepage to install it:
	    https://www.rebar3.org/docs/getting-started
	EOF
	printf "$RESET"
    fi

    if [ ! -d $TEMPLATE_DIR ]; then
	echo "${YELLOW}Rebar3 template directory not found. - creating...${RESET}"
	mkdir -p $TEMPLATE_DIR
    fi

    echo "${BLUE}Copying Nova templates into rebar3 directory: ${TEMPLATE_DIR}${RESET}"
    cp $tmp_dir/nova.template $TEMPLATE_DIR && cp -r $tmp_dir/nova $TEMPLATE_DIR

    if [ $? -ne 0 ]; then
	error "Could not copy template files into directory"
	rm -rf $tmp_dir
	exit 1
    fi
    rm -rf $tmp_dir
}

main() {
    setup_color

    echo "${BLUE}Installing Nova framework templates for rebar3${RESET}"

    if ! command_exists git; then
	error "git is not installed. Please install git before continuing"
	exit 1
    fi

    setup_templates

    if [ $? -eq 0 ]; then
	printf "$YELLOW"
	cat <<-'EOF'
	  _   _			  _____						   _
	 | \ | | _____	 ____ _	 |  ___| __ __ _ _ __ ___   _____      _____  _ __| | __
	 |  \| |/ _ \ \ / / _` | | |_ | '__/ _` | '_ ` _ \ / _ \ \ /\ / / _ \| '__| |/ /
	 | |\  | (_) \ V / (_| | |  _|| | | (_| | | | | | |  __/\ V  V / (_) | |  |   <
	 |_| \_|\___/ \_/ \__,_| |_|  |_|  \__,_|_| |_| |_|\___| \_/\_/ \___/|_|  |_|\_\
			     SIMPLE | FAULT-TOLERANT | DISTRIBUTED
	EOF
	echo " Installation complete. You can now run:${RESET}"
	echo "	 rebar3 new nova my_first_nova"
	echo " ${YELLOW}to create your first Nova-based project OR${RESET}"
	echo "	 rebar3 new nova_rest my_first_nova_rest"
	echo " ${YELLOW}to skip webviews and create a REST-only project${RESET}"
    fi
}


main "$@"
