### libraries
import pandas as pd
import numpy as np
import networkx as nx
import os
import requests


### download data
def download_file(url, path):
    response = requests.get(url)
    response.raise_for_status()
    with open(path, "wb") as f:
        f.write(response.content)


def download_github_data(files):
    print("Downloading data from GitHub Innovation Graph")
    if not os.path.exists("../data"):
        os.makedirs("../data")

    for i in files:
        if not os.path.exists(f"../data/{i}"):
            print(f"{i} not found, downloading...")
            download_file(
                f"https://raw.githubusercontent.com/github/innovationgraph/main/data/{i}",
                f"../data/{i}",
            )
        else:
            print(f"{i} already exists")


### general data preparation
def period_filter(data, years_to_consider):
    """focus on 2020-2023 period only"""
    data = data[data["year"].isin(years_to_consider)]
    return data


def drop_specifics_from_list(data, filter_list):
    """filter specific languages from list -- motivated by RM del Rio-Chanona et al 2023"""
    data = data[~data["language"].str.contains(filter_list, case=False, regex=True)]
    return data


def top_languages_filter(data, nr_languages):
    """keep top x number of languages ONLY"""
    top_languages = (
        data.groupby(["language"])["num_pushers"]
        .agg("mean")
        .reset_index()
        .sort_values(by="num_pushers", ascending=False)
    )
    top_languages = list(top_languages["language"])[:nr_languages]
    data = data[data["language"].isin(top_languages)]
    return data


def drop_country_codes_from_list(data, country_list):
    data = data[~data["iso2_code"].isin(country_list)]
    data = data.dropna(subset="iso2_code")
    return data


def add_period_ids(data, period):
    """create missing semester ID and construct different period IDs"""
    if period == "year":
        year_to_period = dict(
            zip(data["year"].unique(), list(range(1, len(data["year"].unique()) + 1)))
        )
        data["period"] = data["year"].map(year_to_period)
    if period == "semester":
        data["semester"] = np.where(data["quarter"] <= 2, 1, 2)
        data["semester_id"] = (
            data["year"].astype(str).str.cat(data["semester"].astype(str), sep="s")
        )
        semester_to_period = dict(
            zip(
                data["semester_id"].unique(),
                list(range(1, len(data["semester_id"].unique()) + 1)),
            )
        )
        data["period"] = data["semester_id"].map(semester_to_period)
    if period == "quarter":
        data["quarter_id"] = (
            data["year"].astype(str).str.cat(data["quarter"].astype(str), sep="q")
        )
        quarter_to_period = dict(
            zip(
                data["quarter_id"].unique(),
                list(range(1, len(data["quarter_id"].unique()) + 1)),
            )
        )
        data["period"] = data["quarter_id"].map(quarter_to_period)
    return data


def bundle_data(data, periods):
    """aggreagte data for period by taking the mean number active developers"""
    data = (
        data[data["period"].isin(periods)]
        .groupby(["iso2_code", "language"])["num_pushers"]
        .agg("mean")
        .reset_index()
    )
    data["period"] = 1
    data["num_pushers"] = data["num_pushers"].astype(int)
    return data


def rca_calculation(table, c_column, p_column, value_column, threshold):
    """calculate RCA from an M_cp dataframe"""
    table["e_p"] = table.groupby(p_column)[value_column].transform("sum")
    table["e_c"] = table.groupby(c_column)[value_column].transform("sum")
    table["e"] = table[value_column].sum()

    table["rca"] = (table[value_column] / table["e_p"]) / (table["e_c"] / table["e"])
    table["rca01"] = np.where(table["rca"] >= threshold, 1, 0)
    return table


def world_bank_data_cleaner(df, names_list, new_names):
    """function to clean similar tables from the World Bank Databank"""
    filtered_wdf = df[df["variable"].isin(names_list)]

    # melt the dataframe for all variables
    long_df = pd.melt(
        filtered_wdf,
        id_vars=["variable", "country_name", "iso3_code"],
        value_vars=[col for col in filtered_wdf.columns if "YR" in col],
        var_name="year",
        value_name="value",
    )

    # extract the year
    long_df["year"] = long_df["year"].str.extract(r"(\d{4})")

    # pivot to create separate columns for each variable (optional)
    country_df = long_df.pivot_table(
        index=["country_name", "iso3_code", "year"],
        columns="variable",
        values="value",
        aggfunc="first",
    ).reset_index()

    # rename columns
    rename_dict = dict(zip(names_list, new_names))
    country_df.rename(columns=rename_dict, inplace=True)
    country_df["year"] = country_df["year"].astype(int)
    return country_df


def mat_reshape(path, column_labels):
    """to reshape the matrices from Viktor Stojkoski"""
    mat = pd.read_csv(path)
    mat.set_index("Row", inplace=True)
    mat = mat.unstack().reset_index()
    mat.columns = column_labels
    return mat


### relatedness data preparation
def edgelist_cleaning_for_software_space(data, key_columns):
    """get software space network from raw proximity values"""
    data = data[key_columns]

    # drop zero -- non-existing edges
    data = data[data[key_columns[2]] > 0]

    # drop self loops AND keep unique edges ONLY
    data = data[data[key_columns[0]] < data[key_columns[1]]]
    return data


def maximum_spanning_tree(data, key_columns):
    """get the maximum spanning tree of the full relatedness based network"""
    table = data.copy()
    table["distance"] = 1.0 / table[key_columns[2]]
    G = nx.from_pandas_edgelist(
        table,
        source=key_columns[0],
        target=key_columns[1],
        edge_attr=["distance", key_columns[2]],
    )
    T = nx.minimum_spanning_tree(G, weight="distance")
    table2 = nx.to_pandas_edgelist(T)
    table2 = table2[table2[key_columns[2]] > 0]
    table2.rename(
        columns={
            "source": key_columns[0],
            "target": key_columns[1],
            key_columns[2]: "score",
        },
        inplace=True,
    )
    table = pd.merge(table, table2, on=key_columns[0:2])
    table["edge"] = table.apply(
        lambda x: "%s-%s"
        % (
            min(x[key_columns[0]], x[key_columns[1]]),
            max(x[key_columns[0]], x[key_columns[1]]),
        ),
        axis=1,
    )
    table = table.drop_duplicates(subset=["edge"])
    table = table.drop(columns=["edge"])
    return table[key_columns]


def add_edges(mst_edges, all_edges, nr_edges_to_add):
    """add edges to the maximum spanning tree to have a 1/3 nodes/edges ratio"""
    # drop mst edges from the full edgelist
    mst_edges["drop"] = 1
    all_edges = pd.merge(
        all_edges, mst_edges, on=["language_1", "language_2", "proximity"], how="left"
    )
    all_edges = all_edges[all_edges["drop"] != 1].drop(columns="drop")

    # sort and select
    all_edges = all_edges.sort_values(by="proximity", ascending=False).iloc[
        :nr_edges_to_add
    ]

    # add to mst edgelist
    software_space_el = pd.concat([mst_edges, all_edges])
    software_space_el.drop(columns=["drop"], inplace=True)
    return software_space_el
