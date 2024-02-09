# ecoli_mabisi_2022
- **Experiment 1 = repeated E. coli invasion **
- Experiment 2 = propagation of top/bottom mabisi [ignored and not used for any manuscript or thesis chapter!, concerns variable "SPACE"]

# bacterial community profiles
- fall_2022_communities_v1.R (includes description of experimental variables) 
- output_barcoded_mabisi_2plates_0.95_clustering.csv (bionformatic pipeline output: "abundance","cluster","sample","total_count")
- fall_2002_META.csv (match experimental variables to each sequencing barcode: TRANSFER, COMMUNITY, REP, SPACE, SAMPLE, PLATE)
- new_names.csv (for renaming cluster names)
- unique.csv (to define number of unique clusters)
- colours.csv (to assign specific colours for each cluster)

# metabolic profiles 
- fall_2022_gcms_v3.R
- gcms_raw_dec2022.xlsx (includes read.me tab explaining variables)
- aroma_names.csv (full name of aromas)

# acidity
- fall_2022_ph_v2.R 
- fall_2022_ph.xlsx (raw pH values, includes read.me tab explaining variables)
  
# E. coli growing alone in BHI and milk
- feb2024_ecoli_cfus.R
- feb2024_ecoli.xlsx (pH and raw plate counts after 72 hours, includes read.me tab explaining variables)


# variable descriptions:
TREATMENT: transfering from top 20mL (top), bottom 20mL (bottom), normally (con), tube sealed and kept on spinner (mix), ecoli added at every transfer (inv), ecoli line recovering with no ecoli added (rec)

TRANSFER: 0 = day of inoculation. Fermentation regime: 72hrs @ 28 C, backslopped (t1), 24 hrs @ 4 C, 72 hours @28 C, backslopped (t2). Repeat. t02 refers to end-product of t01

REP: replicate 1 to 6

CODE: combination of treatment + replicate

SPACE: area of tube sampled from (i.e., upper 20mL = up, lower 20mL = low, entire tube mixed = all)
