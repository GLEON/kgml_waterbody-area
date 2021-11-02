all: \
data/monthly_timeseries.zip \
data/ReaLSAT.zip
# data/realsat_r_2_monthly_shapes_gis.zip

%.zip:	
	wget -O $@ $(patsubst data/%, http://umnlcc.cs.umn.edu/realsat/data/%, $@)
	Rscript -e "install.packages('archive')"
	Rscript -e "archive::archive_extract('$@', dir = 'data')"	

clean:
	-rm data/monthly_timeseries.zip
