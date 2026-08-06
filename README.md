# Overview
This repository provides code that extracts data from the GoogleEarth Engine API on a weekly basis based on given coordinates. Three requirements need to be met before the code run succesfully.
1. A private service account key needs to be stored in the repository.
2. Currently the code checks if new data was added to data/data_surveylevel_summarized.csv in relation to the data/data_survey_rawgradient.csv file.
3. The Aridity data needs access permission from jakeberger92@gmail.com.

## Incorporating a private service key to Github.
### Creating a key
1. Open the Google Cloud IAM & Admin Console.
2. Click Create Service Account at the top.
3. Give it a name (e.g., github-actions-gee), and click Create and Continue.
5. Click Done.
6. Click on your newly created service account from the list, navigate to the Keys tab, click Add Key, and choose Create new key.
7. Select JSON as the format and click Create. A .json file will automatically download to your computer.


### Adding the key to the GitHub repository.
1. Navigate to your repository on GitHub.
2. Click Settings → Secrets and variables → Actions.
3. Click New repository secret.Name the secret EARTHENGINE_TOKEN.
4. Paste the entire JSON string copied from your local credentials file into the value block and click Add secret.
