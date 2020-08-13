
all: \
data/realsat_r_2_monthly_timeseries.zip \
data/ReaLSAT-R-2.0.zip
# data/realsat_r_2_monthly_shapes_gis.zip

%.zip:
	wget -O $@ $(patsubst data/%, http://umnlcc.cs.umn.edu/realsat/reservoirs/data/%, $@)
	unzip $@ -d data
