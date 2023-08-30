# Create function to get column widths from dataframe
# This function is called later in this script to format the column widths in Excel
def get_col_widths(dataframe, has_index):
    if has_index:
        # First we find the maximum length of the index column
        idx_max = max([len(str(s)) for s in dataframe.index.values] + [len(str(dataframe.index.name))])
        # Then, we concatenate this to the max of the lengths of column name and its values for each column, left to right
        return [idx_max] + [max([len(str(s)) for s in dataframe[col].values] + [len(col)]) for col in dataframe.columns]
    else:
        return [max([len(str(s)) for s in dataframe[col].values] + [len(col)]) for col in dataframe.columns]


# Create function used below for resizing the existing pivot table
def colnum_string(n):
    string = ""
    while n > 0:
        n, remainder = divmod(n - 1, 26)
        string = chr(65 + remainder) + string
    return string
