from itertools import chain
import re

#-------------------- Part 1 --------------------#

def calc_mul_sum(text: str):
    multiplications = filter_mul(text)
    return sum([x * y for x, y in multiplications])

def filter_mul(text: str):
    pattern = r"mul\((\d+),(\d+)\)"
    matches = re.findall(pattern, text)
    return [(int(x), int(y)) for x, y in matches]

#-------------------- Part 2 --------------------#

def calc_do(text: str):
    extracted_content = "".join(re.findall(r"do\(\)(.*?)don't\(\)", "do()" + text.replace('\n', ' ') + "don't()"))
    multiplications = re.findall(r"mul\((\d+),(\d+)\)", extracted_content)
    return sum(int(x) * int(y) for x, y in multiplications)

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        content: str = f.read()

    print(f"Sum of valid multiplications: {calc_mul_sum(content)}")
    print(f"Sum of do mul: {calc_do(content)}")
    
    

    




