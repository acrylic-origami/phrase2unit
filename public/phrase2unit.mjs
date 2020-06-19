import * as rts from "./rts.mjs";
import module from "./phrase2unit.wasm.mjs";
import req from "./phrase2unit.req.mjs";

export const f_inst = module
	.then(m => rts.newAsteriusInstance(Object.assign(req, {module: m})))
	.then(i => {
		// i.exports.hs_init();
		i.exports.main().catch(err => {if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) i.fs.writeSync(2, `phrase2unit: ${err}
		`)});
	}, console.log);
