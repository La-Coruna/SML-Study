fun reverse(xs:int list):int list =
	let
		fun reverse_help(org:int list,res:int list) :int list=
			if null org
			then res
			else reverse_help(tl org, (hd org) :: res);
	in
		reverse_help(xs,[])
	end
;