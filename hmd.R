# HMD credentials
username <- "hanlin.shang@gmail.com"
password <- "hshang85"

# Get population data for 1 Jan 2015
auspop <- hmd.pop(country = "AUS", username = username, password = password, label = "Australia")
auspop <- extract.ages(auspop, age = 0:100)