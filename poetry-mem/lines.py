import csv

out_file = open("poems.csv", "w")
csv_wr = csv.writer(out_file)

num = 1
num_str = str(num)

poem_lines = [
    num_str + ": " + line.strip() for line in open("sonnet" + num_str)]

for call, response in zip([num_str] + poem_lines[:-1], poem_lines):
    csv_wr.writerow([call, response])
