#!/bin/sh

## Nova installation script

TEMPLATE_DIR=$HOME/.config/rebar3/templates

if [ -d $TEMPLATE_DIR ]
then
    echo "Rebar3 template directory found..."
else
    echo "Rebar3 template directory not found. - creating..."
    mkdir -p $TEMPLATE_DIR
fi

echo "Copying Nova templates into rebar3 directory"
cp nova.template $TEMPLATE_DIR && cp -r nova $TEMPLATE_DIR
if [ $? -eq 0 ]; then
    echo "Installation complete. You can now run:"
    echo "  rebar3 new nova my_first_nova"
    echo "This creates a new nova project"
fi
