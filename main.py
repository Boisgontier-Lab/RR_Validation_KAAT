import os
import zipfile
import subprocess

"""
Main function to prepare the data environment and execute an R script.

Steps performed:
1. Initializes variables for the dataset directory and zip file names.
2. Checks if the dataset directory exists; if not, attempts to unzip the dataset from a zip file.
	- If the zip file is missing, prints an error and exits.
3. Writes the absolute path of the dataset directory to a `.env` file as the `DATA_DIR` variable.
4. Attempts to run the R script located at `R/main.R` using `Rscript`.
	- If the R script is missing, prints an error.
	- If an error occurs during execution, prints the error message.
"""
def main():
	# 1. Initialize variables
	filename = '2018_cheval_code_and_data'
	zip_filename = '2018_cheval_code_and_data.zip'

	# 2. Unzip data set if not already unzipped
	if not os.path.exists(filename):
		if os.path.exists(zip_filename):
			with zipfile.ZipFile(zip_filename, 'r') as zip_ref:
				zip_ref.extractall('.')
		else:
			print(f"The file {zip_filename} is not found.")
			return
	abs_path = os.path.abspath(filename)

	# 3. Write the absolute path to .env
	with open('.env', 'w') as f:
		f.write(f"DATA_DIR={abs_path}\n")

	# 4. Run the R script R/main.R
	r_script = os.path.join('R', 'main.R')
	if os.path.exists(r_script):
		try:
			subprocess.run(['Rscript', r_script], check=True)
		except Exception as e:
			print(f"Error while running the R script: {e}")
	else:
		print(f"The script {r_script} is not found.")

if __name__ == "__main__":
	main()
