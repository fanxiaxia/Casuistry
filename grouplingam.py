import numpy as np
import pandas as pd
import graphviz
import lingam
from lingam.utils import print_causal_directions, print_dagc, make_dot

print([np.__version__, pd.__version__, graphviz.__version__, lingam.__version__])

np.set_printoptions(precision=3, suppress=True)
np.random.seed(0)

data_list = []

for lp in range(1,25):
    print(f"this is irg {lp}!")
    filename = f"datairg{lp}.csv"

    # Load the data from the CSV file into a pandas DataFrame
    df = pd.read_csv(filename)

    # Get the number of columns of the DataFrame
    num_columns = df.shape[1]

    # Print the number of columns
    print("Number of columns:", num_columns)

    #print(df)

    df2 = df.iloc[:, 1:7]

    #print(df2)

    # Convert the pandas DataFrame to a NumPy array
    data = np.array(df2)

    data_list.append(data)

    #print(data)

model = lingam.MultiGroupDirectLiNGAM()
model.fit(data_list)

print(model.causal_order_)

results = model.bootstrap(data_list, n_sampling=1000)
for lp in range(0,24):
    data_temp = data_list[lp]
    cdc = results[lp].get_causal_direction_counts(min_causal_effect=0.01)
    print_causal_directions(cdc, 1000)
    df = pd.DataFrame(cdc)
    labels = [f'{i}' for i in range(data_temp.shape[1])]
    df['from'] = df['from'].apply(lambda x: labels[x])
    df['to'] = df['to'].apply(lambda x: labels[x])
    df.sort_values('count', ascending=False)
    print(df)
    df.to_csv(f'group_bootsirg{lp}.csv')

