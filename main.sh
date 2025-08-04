#!/bin/bash

district="$1"

Rscript main.R $district
Rscript main.R "${district}-post"
python scripts/csv2excel.py $district
python scripts/csv2excel.py "${district}-post"
