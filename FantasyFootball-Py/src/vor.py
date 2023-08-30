import pandas as pd
import numpy as np
from excelFormatting import get_col_widths


fantasyFootballFolder = r'Z:\Fantasy Football'

# leagueSettings = [[14, 1, 2, 2, 1, [['QB', 'RB', 'WR', 'TE']], 5],
#                   [1 / 25, 1 / 25, 3.25, -2.75, .15, 1 / 10, 6, .75, 1 / 10, 6, -2]]
leagueSettings = [[18, 1, 1, 1, 0, [[0, 'RB', 'WR', 0], [0, 'RB', 'WR', 0], [0, 0, 'WR', 'TE'], [0, 0, 'WR', 'TE']], 8],
                  [.1, 1 / 25, 6, -2, .2, 1 / 10, 6, .5, 1 / 10, 6, -2]]


qbProjections = pd.read_csv(fantasyFootballFolder + r'\Projections\FantasyPros_Fantasy_Football_Projections_QB.csv',
                            skip_blank_lines=True, thousands=',').dropna()
rbProjections = pd.read_csv(fantasyFootballFolder + r'\Projections\FantasyPros_Fantasy_Football_Projections_RB.csv',
                            skip_blank_lines=True, thousands=',').dropna()
wrProjections = pd.read_csv(fantasyFootballFolder + r'\Projections\FantasyPros_Fantasy_Football_Projections_WR.csv',
                            skip_blank_lines=True, thousands=',').dropna()
teProjections = pd.read_csv(fantasyFootballFolder + r'\Projections\FantasyPros_Fantasy_Football_Projections_TE.csv',
                            skip_blank_lines=True, thousands=',').dropna()
qbProjections.rename(columns={'ATT': 'PaATT',
                              'YDS': 'PaYDS',
                              'TDS': 'PaTDS',
                              'ATT.1': 'RuATT',
                              'YDS.1': 'RuYDS',
                              'TDS.1': 'RuTDS'}, inplace=True)
qbProjections['Pos'] = 'QB'
rbProjections.rename(columns={'ATT': 'RuATT',
                              'YDS': 'RuYDS',
                              'TDS': 'RuTDS',
                              'YDS.1': 'ReYDS',
                              'TDS.1': 'ReTDS'}, inplace=True)
rbProjections['Pos'] = 'RB'
wrProjections.rename(columns={'ATT': 'RuATT',
                              'YDS': 'ReYDS',
                              'TDS': 'ReTDS',
                              'YDS.1': 'RuYDS',
                              'TDS.1': 'RuTDS'}, inplace=True)
wrProjections['Pos'] = 'WR'
teProjections.rename(columns={'YDS': 'ReYDS',
                              'TDS': 'ReTDS'}, inplace=True)
teProjections['Pos'] = 'TE'
projections = pd.concat([qbProjections, rbProjections, wrProjections, teProjections])
projections = projections[
    ['Player', 'Team', 'Pos', 'PaATT', 'CMP', 'PaYDS', 'PaTDS', 'INTS', 'RuATT', 'RuYDS', 'RuTDS', 'REC', 'ReYDS',
     'ReTDS', 'FL']]

projections['FPTS'] = (projections.iloc[:, 4:].astype(float) * leagueSettings[1]).sum(axis=1)
projections['FPPG'] = projections['FPTS'] / 17
projections['Pos Rank'] = projections.groupby('Pos')['FPTS'].rank(ascending=False)
projections['Total Rank'] = projections['FPTS'].rank(ascending=False)

nonStarters = projections[
    ((projections['Pos Rank'] > leagueSettings[0][0] * leagueSettings[0][1]) & (projections['Pos'] == 'QB')) |
    ((projections['Pos Rank'] > leagueSettings[0][0] * leagueSettings[0][2]) & (projections['Pos'] == 'RB')) |
    ((projections['Pos Rank'] > leagueSettings[0][0] * leagueSettings[0][3]) & (projections['Pos'] == 'WR')) |
    ((projections['Pos Rank'] > leagueSettings[0][0] * leagueSettings[0][4]) & (projections['Pos'] == 'TE'))].copy()

# nonStarters.sort_values('Total Rank', inplace=True)
temp = leagueSettings[0][6:10]
i = 0
for flexSpot in leagueSettings[0][5]:
    flex = pd.DataFrame()
    if i == 0:
        nonFlex = pd.DataFrame()
        if flexSpot[0] == 'QB':
            flex = pd.concat([flex, nonStarters[nonStarters['Pos'] == 'QB'].copy()])
        else:
            nonFlex = pd.concat([nonFlex, nonStarters[nonStarters['Pos'] == 'QB'].copy()])

        if flexSpot[1] == 'RB':
            flex = pd.concat([flex, nonStarters[nonStarters['Pos'] == 'RB'].copy()])
        else:
            nonFlex = pd.concat([nonFlex, nonStarters[nonStarters['Pos'] == 'RB'].copy()])

        if flexSpot[2] == 'WR':
            flex = pd.concat([flex, nonStarters[nonStarters['Pos'] == 'WR'].copy()])
        else:
            nonFlex = pd.concat([nonFlex, nonStarters[nonStarters['Pos'] == 'WR'].copy()])

        if flexSpot[3] == 'TE':
            flex = pd.concat([flex, nonStarters[nonStarters['Pos'] == 'TE'].copy()])
        else:
            nonFlex = pd.concat([nonFlex, nonStarters[nonStarters['Pos'] == 'TE'].copy()])
    else:
        if flexSpot[0] == 'QB':
            flex = pd.concat([flex, nonFlex[nonFlex['Pos'] == 'QB'].copy()])
            nonFlex = nonFlex[nonFlex['Pos'] != 'QB']

        if flexSpot[1] == 'RB':
            flex = pd.concat([flex, nonFlex[nonFlex['Pos'] == 'RB'].copy()])
            nonFlex = nonFlex[nonFlex['Pos'] != 'RB']

        if flexSpot[2] == 'WR':
            flex = pd.concat([flex, nonFlex[nonFlex['Pos'] == 'WR'].copy()])
            nonFlex = nonFlex[nonFlex['Pos'] != 'WR']

        if flexSpot[3] == 'TE':
            flex = pd.concat([flex, nonFlex[nonFlex['Pos'] == 'TE'].copy()])
            nonFlex = nonFlex[nonFlex['Pos'] != 'TE']

    flex.sort_values('Total Rank', inplace=True)
    flex = flex.iloc[leagueSettings[0][0]:]

    nonFlex = pd.concat([nonFlex, flex])

    i += 1

nonFlex.sort_values('Total Rank', inplace=True)
nonBench = nonFlex.iloc[(leagueSettings[0][0] * leagueSettings[0][6]):].copy()

qbReplacementValue = nonFlex[nonFlex['Pos'] == 'QB']['FPTS'].max()
rbReplacementValue = nonFlex[nonFlex['Pos'] == 'RB']['FPTS'].max()
wrReplacementValue = nonFlex[nonFlex['Pos'] == 'WR']['FPTS'].max()
teReplacementValue = nonFlex[nonFlex['Pos'] == 'TE']['FPTS'].max()

qbReplacementValue = nonBench[nonBench['Pos'] == 'QB']['FPTS'].max()
rbReplacementValue = nonBench[nonBench['Pos'] == 'RB']['FPTS'].max()
wrReplacementValue = nonBench[nonBench['Pos'] == 'WR']['FPTS'].max()
teReplacementValue = nonBench[nonBench['Pos'] == 'TE']['FPTS'].max()

conditions = [projections['Pos'] == 'QB',
              projections['Pos'] == 'RB',
              projections['Pos'] == 'WR',
              projections['Pos'] == 'TE']

replacementValues = [projections['FPTS'] - qbReplacementValue,
          projections['FPTS'] - rbReplacementValue,
          projections['FPTS'] - wrReplacementValue,
          projections['FPTS'] - teReplacementValue]

benchValues = [projections['FPTS'] - qbReplacementValue,
          projections['FPTS'] - rbReplacementValue,
          projections['FPTS'] - wrReplacementValue,
          projections['FPTS'] - teReplacementValue]

projections['VOR'] = np.select(conditions, values)
projections.sort_values('VOR', ascending=False, inplace=True)
projections.reset_index(drop=True, inplace=True)
projections.index = projections.index + 1
projections.reset_index(inplace=True)

projections.rename(columns={'index': 'Pick'}, inplace=True)
projections['Round'] = projections['Pick'] / 14
projections['Round'] = projections['Round'].apply(np.ceil)

projections = projections[
    ['Round', 'Pick', 'Player', 'Team', 'Pos', 'Pos Rank', 'Total Rank', 'VOR', 'FPTS', 'FPPG', 'PaATT', 'CMP', 'PaYDS', 'PaTDS', 'INTS', 'RuATT', 'RuYDS', 'RuTDS', 'REC', 'ReYDS',
     'ReTDS', 'FL']]

# Define output path
outputPath = fantasyFootballFolder + r'\VOR.xlsx'

# Create Excel connection and write dataframes to Excel file
with pd.ExcelWriter(outputPath, engine='xlsxwriter') as writer:
    # Export dataframes to Excel
    projections.to_excel(writer, sheet_name='VOR', index=False)

    # Format all column widths on both sheets
    for i, width in enumerate(get_col_widths(projections, False)):
        writer.sheets['VOR'].set_column(i, i, width)

    # Freeze rows/columns on
    writer.sheets['VOR'].freeze_panes(1, 3)

# fantasyFootballFolder = r'Z:\Fantasy Football'
#
# # Step 0: Input and clean data
# roster = pd.read_parquet(fantasyFootballFolder + r'\Database\roster.parquet')
# playerStatsYearly = pd.read_parquet(fantasyFootballFolder + r'\Database\playerStatsYearly.parquet')
#
# playerStatsYearly = playerStatsYearly[playerStatsYearly['league'] == 'NFL']

print('test')
