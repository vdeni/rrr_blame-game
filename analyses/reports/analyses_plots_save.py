names = ['s2b_comparison.png',
         's2a_comparison.png',
         's1b_comparison.png',
         's1a_comparison.png']

with open('reports/analyses_plots_create.R',
          'r') as infile:
    script = infile.readlines()

found = False

with open('tmp.R',
          'w') as outfile:
    for line in script:
        if not (line.startswith('s1Plot') or line.startswith('s2Plot')) and\
                found is False:
            outfile.write(line)
        elif (line.startswith('s1Plot') or line.startswith('s2Plot')) and\
                found is False:
            outfile.write(line)
            found = True
            continue
        elif not (line.startswith('s1Plot') or line.startswith('s2Plot')) and\
                found is True:
            outfile.write(line)
            outfile.write(f'''ggsave(filename = "reports/plots/{names.pop()}",
                                     device = "png",
                                     dpi = 300,
                                     bg = '#ffffff')
                          ''')
            found = False
