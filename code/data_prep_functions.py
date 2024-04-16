def drop_specifics_from_list(data, filter_list):
    """filter specific languages from list -- motivated by RM del Rio-Chanona et al 2023"""
    data = data[~data["language"].str.contains(filter_list, case=False, regex=True)]
    return data

def top_languages_filter(data, nr_languages):
    """keep top x number of languages ONLY"""
    top_languages = data.groupby(["language"])["num_pushers"].agg("mean").reset_index().sort_values(by="num_pushers", ascending=False)
    top_languages = list(top_languages["language"])[:nr_languages]
    data = data[data["language"].isin(top_languages)]
    return data
    
def drop_country_codes_from_list(data, country_list):
    data = data[~data["iso2_code"].isin(country_list)]
    data = data.dropna(subset="iso2_code")
    return data

def add_period_ids(data, period):
    """create missing semester ID and construct different period IDs"""
    if period=="year":
        year_to_period = dict(zip(data["year"].unique(), list(range(1, len(data["year"].unique()) + 1))))
        data["period"] = data["year"].map(year_to_period)
    if period=="semester":
        data["semester"] = np.where(data["quarter"] <= 2, 1, 2)
        data["semester_id"] = data["year"].astype(str).str.cat(data["semester"].astype(str), sep="s")
        semester_to_period = dict(zip(data["semester_id"].unique(), list(range(1, len(data["semester_id"].unique()) + 1))))
        data["period"] = data["semester_id"].map(semester_to_period)
    if period=="quarter":
        data["quarter_id"] = data["year"].astype(str).str.cat(data["quarter"].astype(str), sep="q")
        quarter_to_period = dict(zip(data["quarter_id"].unique(), list(range(1, len(data["quarter_id"].unique()) + 1))))
        data["period"] = data["quarter_id"].map(quarter_to_period)
    return data