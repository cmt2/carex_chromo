
#!/bin/bash
#SBATCH --partition=shared-long
#SBATCH --threads-per-core=1
#SBATCH --mem=1024M
#SBATCH --mail-user=cmt2@berkeley.edu
#SBATCH --mail-type=ALL
#SBATCH --n=1
#SBATCH --cpus-per-task=1
#SBATCH --time=01:00:00

 module load devel/Boost

 ./rb scripts/script_ChromoSSE_Carex.Rev
