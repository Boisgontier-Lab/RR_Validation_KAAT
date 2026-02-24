import os
import zipfile
import subprocess

def main():
	# 1. Unzip data set if not already unzipped
	zip_filename = '2018_cheval_code_and_data.zip'
	extract_dir = '2018_cheval_code_and_data'
	if not os.path.exists(extract_dir):
		if os.path.exists(zip_filename):
			with zipfile.ZipFile(zip_filename, 'r') as zip_ref:
				zip_ref.extractall(extract_dir)
		else:
			print(f"The file {zip_filename} is not found.")
			return
	abs_path = os.path.abspath(extract_dir)

	# 2. Write the absolute path to .env
	with open('.env', 'w') as f:
		f.write(f"DATA_DIR={abs_path}\n")

	# 3. Run the R script R/main.R
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
