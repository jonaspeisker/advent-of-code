import pandas as pd

def get_password(file):
    df = pd.read_csv(file, header=None, names=["turn"])
    df["dir"] = df["turn"].str[0]
    df["dist"] = df["turn"].str.extract(r"(\d+)").astype(int)

    counter = 0
    position = 50

    for _, row in df.iterrows():
        direction = row["dir"]
        distance = row["dist"]

        if direction == "L":            # left
            pos_tmp = position - distance
            if pos_tmp >= 0:
                position = pos_tmp
            else:
                position = pos_tmp + 100
        else:                           # right
            pos_tmp = position + distance
            if pos_tmp <= 99:
                position = pos_tmp
            else:
                position = pos_tmp - 100

        # increment counter whenever position == 0
        if position == 0:
            counter += 1

    print(f"The password is {counter}")

get_password("day1/input_example.txt")
get_password("day1/input.txt")
