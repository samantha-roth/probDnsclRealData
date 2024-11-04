#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10gb
#SBATCH --partition=open

module load anaconda3/2021.05

cd /storage/work/svr5482/probDnsclRealData/plots

conda env create --file probDnscl_env.yml --prefix /storage/work/svr5482/probDnsclRealData
ln -s /storage/work/svr5482/probDnsclRealData/probDnscl_env ~/.conda/envs/probDnscl_env

conda deactivate
