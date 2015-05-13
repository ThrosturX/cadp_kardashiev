{application, solar,
	[{description, 
		"Kardashiev Space Trading & Strategy Game.\n 
		By:\n 
		Björn Ingi Baldvinsson,\n
		Jón Reginbald,\n
		Stefanía Bergljót Stefánsdóttir and\n
		Þröstur Thorarensen.\n
		Implemented in Erlang as a part of\n
		CADP-2015 @ Reykjavik University\n"},
	{vsn, "1.0.0"},
	{modules, [solar_system, 
		arbitrator, client]},
	{registered, [solar, solar_system, refresher]},
	{mod, 
		{solar, []}
	}]
}.
