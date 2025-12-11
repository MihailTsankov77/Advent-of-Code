#!/bin/bash

rustc $1
name=$(echo $1 |  cut -d'.' -f 1 | cut -d'/' -f 2)
./$name
rm $name
