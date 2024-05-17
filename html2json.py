from bs4 import BeautifulSoup
import sys


def extract_last_script_content(html_file_path):
    # Read the HTML file
    with open(html_file_path, 'r', encoding='utf-8') as file:
        html_content = file.read()
    
    # Parse the HTML content using BeautifulSoup
    soup = BeautifulSoup(html_content, 'html.parser')
    
    # Find the first <script> tag
        
    element_list = soup.find_all('script')
    return element_list[1].string or ""
    

# Example usage
# html_file_path = 'path/to/your/file.html'
# print(extract_first_script_content(html_file_path))


def trim(contents):
    contents_parts = contents.split(" ", 2)
    contents = contents_parts[2].rstrip(";")
    return contents

source_file = sys.argv[1]
destination_file = sys.argv[2]



content = extract_last_script_content(source_file)
content = trim(content)

f = open(destination_file, "w")
f.write(content)
f.close()
