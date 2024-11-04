#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10gb
#SBATCH --partition=open

module load anaconda3/2021.05

cd /storage/work/svr5482/probDnsclRealData/plots

conda env create --file plot_env.yml --prefix /storage/work/svr5482/probDnsclRealData/plots
ln -s /storage/work/svr5482/probDnsclRealData/plots/plot_env ~/.conda/envs/plot_env

conda deactivate
