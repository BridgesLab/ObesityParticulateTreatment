## Data Pull 

Needed two separate data pulls, one for control one for pneumonia cases.  Both start with 5,529,222 patients, of which 111,200 are MGI participants.  Both were pulled using the datadirect tool.  This was most recently done on 2026-02-08

### All Patients

- On the data pull excluded patients with any (viral or bacterial) pneumonia diagnoses (14,525) leaving 96,675 patients
- In the analysis excluded 

### Pneumonia Patients

- 2690 patients with bacterial pneumonia

## ICD Codes

Diabetes: ICD9 CM 250/ICD10-CM-E10 and E11.  All ICD9 are 250.x, type II or type I specified.  ICD10 E10 is Type I, E11 is Type II
Obesity: ICD9 278/ICD10 CM E66 - used Elixhauser
COPD: ICD9 496/ICD-10 J44, only 492
Heart failure: ICD9 428/ICD10 150, used 428.x and I50.x
Chronic kidney disease: ICD9 585 /ICD10 N18, used 585.x and N18.x
Chronic liver disease: ICD9 571/ICD10 K70-K77 used 571.x, also in Charlson as MildLiverDisease or ModerateSevereLiverDisease or in Elixhauser as LiverDisease
Dysphagia: ICD9 787.2/ICD10 R13.1 (cause of aspiration pneumonia) used 787.2x and R13.1x
Active malignancy:ICD9 40-208/ICD10-C00-C96 (leave out if too complicated)

### Pneumonia diagnosis filter

02.22 or A21.2 or A20.2 or A22.1 or A31.0 or A01.03 or A37.81 or A43.0 or A37.91 or A48.1 or A54.84 or A37.11 or A37.01 or B01.2 or B05.2 or A52.72 or A50.04 or B25.0 or B06.81 or B37.1 or B38.0 or B38.2 or B38.1 or B39.0 or B39.2 or B58.3 or B39.1 or B59 or J09.X1 or B77.81 or J10.00 or J10.08 or J10.01 or J11.00 or J11.08 or J12.0 or J12.2 or J12.1 or J12.81 or J12.3 or J12.82 or J12.89 or J12.9 or J15.1 or J15.20 or J15.0 or J13 or J14 or J15.212 or J15.211 or J15.3 or J15.29 or J15.5 or J15.8 or J15.4 or J15.7 or J15.6 or J15.9 or J16.0 or J16.8 or J18.0 or J18.8 or J17 or J18.1 or J85.1 or J95.851 or J18.9 or 003.22 or 020.3 or 020.4 or 020.5 or 021.2 or 022.1 or 031.0 or 039.1 or 052.1 or 055.1 or 073.0 or 083.0 or 112.4 or 114.0 or 114.4 or 115.05 or 115.95 or 130.4 or 114.5 or 136.3 or 115.15 or 480.0 or 480.9 or 481 or 480.3 or 480.8 or 480.1 or 480.2 or 482.0 or 482.1 or 482.2 or 482.3 or 482.32 or 482.31 or 482.30 or 482.39 or 482.4 or 482.41 or 482.40 or 482.49 or 482.8 or 482.81 or 482.82 or 482.83 or 482.84 or 482.89 or 482.9 or 483 or 483.0 or 483.1 or 484.5 or 484.1 or 484.3 or 484.6 or 483.8 or 484.7 or 484.8 or 485 or 486 or 513.0 or 517.1

Decided to not include these in the bacterial pneumonia pull as they may or may not be bacterial
J18.0 - Bronchopneumonia, unspecified organism (can be bacterial)
J18.8 - Other pneumonia, unspecified organism (can be bacterial)
J18.9 - Pneumonia, unspecified organism (can be bacterial)
485 - Bronchopneumonia, organism unspecified (often bacterial)
486 - Pneumonia, organism unspecified (often bacterial)