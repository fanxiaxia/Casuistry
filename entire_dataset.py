import numpy as np
import pandas as pd
import graphviz
import lingam
import csv
from lingam.utils import print_causal_directions, print_dagc, make_dot

def save_causal_directions_to_file(filename, cdc, n_sampling, labels=None):
    """Print causal directions of bootstrap result to stdout.

    Parameters
    ----------
    cdc : dict
        List of causal directions sorted by count in descending order.
        This can be set the value returned by ``BootstrapResult.get_causal_direction_counts()`` method.
    n_sampling : int
        Number of bootstrapping samples.
    labels : array-like, optional (default=None)
        List of feature lables.
        If set labels, the output feature name will be the specified label.
    """
    with open(filename, 'w') as local_f:
        local_f.write('hi there\n')  # python will convert \n to os.linesep
        for i, (fr, to, co) in enumerate(zip(cdc["from"], cdc["to"], cdc["count"])):
            sign = "" if "sign" not in cdc else "(b>0)" if cdc["sign"][i] > 0 else "(b<0)"
            if labels:
                local_f.write(f"{labels[to]} <--- {labels[fr]} {sign} ({100 * co / n_sampling:.1f}%)\n")
            else:
                local_f.write(f"x{to} <--- x{fr} {sign} ({100 * co / n_sampling:.1f}%)\n")


print([np.__version__, pd.__version__, graphviz.__version__, lingam.__version__])

np.set_printoptions(precision=3, suppress=True)
np.random.seed(100)

df = pd.read_csv("entire_dataset.csv")

# Get the number of columns of the DataFrame
num_columns = df.shape[1]

# Print the number of columns
print("Number of columns:", num_columns)

#print(df)

df2 = df.iloc[:, 1:7]

#print(df2)

# Convert the pandas DataFrame to a NumPy array
dataall = np.array(df2)


model = lingam.DirectLiNGAM()
model.fit(dataall)

print(model.causal_order_)
print(model.adjacency_matrix_)

model = lingam.DirectLiNGAM()
result = model.bootstrap(dataall, n_sampling=1000)

cdc = result.get_causal_direction_counts(min_causal_effect=0.01, split_by_causal_effect_sign=True)
print_causal_directions(cdc, 1000)

# Assign to pandas.DataFrame for pretty display
df = pd.DataFrame(cdc)
labels = [f'{i}' for i in range(dataall.shape[1])]
df['from'] = df['from'].apply(lambda x: labels[x])
df['to'] = df['to'].apply(lambda x: labels[x])
df.sort_values('count', ascending=False)
print(df)
df.to_csv(f'bootsirg_all.csv')
