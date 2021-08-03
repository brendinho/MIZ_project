# -*- coding: utf-8 -*-
"""
Example of data description file: data.py
@author: karlen
"""


def get_data_description():
    """ Define data provided by Esri Canada
    """
    data = {}
    data['nation'] = 'Canada'
    data['description'] = 'Canada by province'
    data['source'] = 'Esri Canada, virihealth.com, and infobase canada'
    data['source_url'] = 'https://resources-covid19canada.hub.arcgis.com/' \
                         ' and https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv'

    # common regional abbreviations used in the data files
    regional_abbreviations = {
        'BC': 'BC',
        'Alberta': 'AB',
        'Saskatchewan': 'SK',
        'Manitoba': 'MB',
        'Ontario': 'ON',
        'Quebec': 'QC',
        'New Brunswick': 'NB',
        'Newfoundland': 'NL',
        'Nova Scotia': 'NS',
        'PEI': 'PE',
        'Yukon': 'YT',
        'NWT': 'NT',
        'Nunavut': 'NU'
    }

    files_data = {}
    filenames = ['ca-pypm.csv','ca-vacc-pypm.csv']
    for filename in filenames:
        file_data = {}
        file_data['source'] = ''
        file_data['date header'] = 'Date'
        file_data['date start'] = [2020, 3, 1]

        files_data[filename] = file_data
    data['files'] = files_data

    # for each region, define location of each population data (file/column header)
    # either daily or total (or both) data can be provided

    regions_data = {}
    for region in regional_abbreviations:

        populations_data = {}

        f0_populations = ['reported', 'deaths']
        f1_populations = ['vaccinated']

        filename = filenames[0]
        for population in f0_populations:
            pop_data_total = {}
            pop_data_total['filename'] = filename
            header = ''
            if population == 'reported':
                header = regional_abbreviations[region] + '-pt'
            if population == 'deaths':
                header = regional_abbreviations[region] + '-dt'

            pop_data_total['header'] = header

            pop_data_daily = {}
            pop_data_daily['filename'] = filename
            header = ''
            if population == 'reported':
                header = regional_abbreviations[region] + '-pd'
            if population == 'deaths':
                header = regional_abbreviations[region] + '-dd'

            pop_data_daily['header'] = header

            population_data = {'total': pop_data_total, 'daily': pop_data_daily}
            populations_data[population] = population_data

        f0_populations = ['in_hospital', 'in_icu']
        filename = filenames[0]
        for population in f0_populations:
            pop_data_total = {}
            pop_data_total['filename'] = filename
            header = ''

            if population == 'in_hospital':
                header = regional_abbreviations[region] + '-ht'
            if population == 'in_icu':
                header = regional_abbreviations[region] + '-it'

            pop_data_total['header'] = header

            population_data = {'total': pop_data_total}
            populations_data[population] = population_data

        filename = filenames[1]
        for population in f1_populations:
            pop_data_total = {}
            pop_data_total['filename'] = filename
            header = ''
            if population == 'vaccinated':
                header = regional_abbreviations[region] + '-xt'

            pop_data_total['header'] = header

            population_data = {'total': pop_data_total}
            populations_data[population] = population_data

        regions_data[region] = populations_data

    data['regional_data'] = regions_data

    return data
