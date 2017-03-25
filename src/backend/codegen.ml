module L = Llvm

let translate sast =
	let context = L.global_context () in
	L.create_module context "shux"
