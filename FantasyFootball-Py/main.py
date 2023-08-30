import subprocess
import shutil

print('Script Started')

# Record required packages in requirements.txt using pipreqs
with open('requirements.txt', 'w') as file_:
    subprocess.Popen(['pip', 'freeze', '-l'], stdout=file_).communicate()
shutil.move('requirements.txt', '../requirements.txt')
print('requirements.txt Saved')

# Backup Project Folder
#if input('Do you want to backup project (y/n)?') == 'y':
#    src = r'C:\Users\S290203\Documents\Python\License Usage Report'
#    dst = r'C:\Users\S290203\OneDrive - AEP\Programming Resources\Python\Project Backups\License Usage Report'
#    shutil.copytree(src, dst, dirs_exist_ok=True)
#    print('Files Backed Up')

# Call script
# exec(open('test.py').read())

print('Script Complete')
