#!/bin/bash
#SBATCH --job-name=chromoSSE_anc_states
#SBATCH --time=14-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --account=zenilab
#SBATCH --partition=zenilab
#SBATCH --ntasks=4
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=cmt2@hawaii.edu


module load devel/Boost/1.69.0-GCCcore-6.3.0
module load toolchain/intel/2018.5.274

cd carex_chromo

./rb scripts/withoutSiderostica_noRHO_ancstates/tp_ChromoSSE_carex_dirichlet_withoutSiderosticta_noRHO_anc_states1.Rev &
./rb scripts/withoutSiderostica_noRHO_ancstates/tp_ChromoSSE_carex_dirichlet_withoutSiderosticta_noRHO_anc_states2.Rev &
./rb scripts/withoutSiderostica_noRHO_ancstates/tp_ChromoSSE_carex_dirichlet_withoutSiderosticta_noRHO_anc_states3.Rev &
./rb scripts/withoutSiderostica_noRHO_ancstates/tp_ChromoSSE_carex_dirichlet_withoutSiderosticta_noRHO_anc_states4.Rev;
wait



