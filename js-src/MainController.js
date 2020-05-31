import React from 'react';
import Q from 'q';

const SUPERLATIVE_LIMS = [1E-9, 1E9];
const superlatives = {
	"area":["pretty big","pretty small"],
	"cent":["pretty expensive","pretty cheap"],
	"charge":["pretty charged","pretty neutral"],
	"chemical amount":["a lot","not a lot"],
	"density":["pretty dense","pretty Light"],
	"energy":["pretty energetic","not very energetic"],
	"energy per chemical amount":["pretty energy-packed", "not very energy-packed"],
	"energy per unit length":["pretty energy-packed", "not very energy-packed"],
	"energy per unit mass":["pretty energy-packed", "not very energy-packed"],
	"energy per unit volume":["pretty energy-packed", "not very energy-packed"],
	"exhaust emission":["pretty polluting","pretty green"],
	"flow":["a gush","a trickle"],
	"force":["pretty forceful","pretty acerbic"],
	"fuel efficiency":["pretty great","pretty terrible"],
	"length":["pretty long","pretty short"],
	"linear density":["pretty dense","pretty light"],
	"luminous intensity":["pretty bright","pretty dim"],
	"magnetic field strength":["pretty strong","pretty weak"],
	"mass":["pretty heavy","pretty light"],
	"mass per unit power":["pretty weak","pretty strong"],
	"mass per unit time":["a gush","a trickle"],
	"molar rate":["a gush","a trickle"],
	"power":["pretty powerful","pretty weak"],
	"power per unit mass":["pretty powerful","pretty weak"],
	"radioactivity":["pretty dangerous","nothing to worry about"],
	"speed":["pretty fast","pretty slow"],
	"temperature":["pretty hot","pretty cold"],
	"time":["pretty long","pretty short"],
	"torque":["pretty strong","pretty weak"],
	"volume":["pretty big","pretty small"]
}
function sgn(x) { return x > 0 ? 1 : -1; }

export default class extends React.Component {
	constructor(props) {
		super(props);
		this.form_ref = React.createRef();
		this.term_raw_ref = React.createRef();
		this.uri_stash_ref = React.createRef();
		this.state = {
			result: null,
			n_request: 0,
			n_fulfilled: 0,
			breakdown_type: 'raw',
			term_raw: '',
			request: null,
			err: null,
			copying: false
		};
		window.addEventListener('popstate', this.handle_uri_term);
	}
	
	componentDidMount() {
		this.term_raw_ref.current.focus();
		this.handle_uri_term();
	}
	handle_uri_term = () => {
		const term_raw = (new URLSearchParams(window.location.search)).get('term_raw');
		if(term_raw != null) {
			this.term_raw_ref.current.value = term_raw;
			this.setState(s => ({ term_raw, n_request: s.n_request + 1 }));
		}
	}
	componentDidUpdate(_, l) {
		if(this.state.err !== null && !this.state.err[1]) {
			const this_err = this.state.err[0];
			this.setState(({ err }) => err !== null && (err[0] === this_err ? { err: [this_err, true] } : {}));
			setTimeout(_ => this.setState(({ err }) => err !== null && (err[0] === this_err ? { err: null } : {})), 4000);
		}
		if(this.state.copying)
			setTimeout(_ => this.setState({ copying: false }), 1000);
		
		if(l.n_request != this.state.n_request) {
			const n_request_stash = this.state.n_request
			const term_raw = this.term_raw_ref.current.value;
			const fail = e => {
				console.log(e);
				this.setState(s => (n_request_stash === s.n_request) && { err: [e.message, false], n_fulfilled: n_request_stash });
			}
			if(term_raw.trim().length > 0)
				fetch(`/end/q`, { method: 'POST', body: new FormData(this.form_ref.current) })
					.then((res) => {
						if(res.ok)  
							return res.text().then(t => {
								if(t.trim() === 'nothing')
									throw new Error('Could not find any units in phrase.');
								else return JSON.parse(t)
							});
						else
							return res.text().then(t => {
								throw new Error(t);
							});
					})
					.then(res => {
						// debugger;
						this.setState(s => (n_request_stash === s.n_request) && { result: res, n_fulfilled: n_request_stash })
					})
					.catch(fail);
			else fail(new Error('Enter a non-empty phrase.'))
		}
	}
	
	onTermChange = e => this.setState({ term_raw: e.target.value });
	
	onChangeBreakdownType = e => this.setState({ breakdown_type: e.target.value });
	
	onSubmit = e => {
		history.pushState({}, `search`, `?term_raw=${encodeURI(this.term_raw_ref.current.value)}`);
		this.setState(({ n_request }) => ({ n_request: n_request + 1 }));
		e.preventDefault();
	}
	
	copyURI = () => {
		const past_focus = document.activeElement;
		console.log(this.uri_stash_ref.current.value);
		this.uri_stash_ref.current.select();
		document.execCommand('copy');
		past_focus.focus();
		this.setState({ copying: true });
	}
	
	pprunit = u => {
		const units = Object.entries(u);
		units.sort((a, b) => a[1] < b[1]);
		const terms = units.map(([name, power], idx) => 
			<span key={idx}>{power < 0 ? '/' : (idx > 0 && (<span>&middot;</span>))}{name}<sup>{Math.abs(power) > 1 && Math.abs(power)}</sup></span>
		);
		return <span className="unit-list">{terms.length === 0 ? ' <unitless>' : terms}</span>;
	}
		
	// term_mouse = (e, i) => {
	// 	const e_type = e.type; // grr event pooling
	// 	this.setState(s => {
	// 		const s_ = s.shown.slice();
	// 		s_[i] = e_type === 'mouseenter';
	// 		return {
	// 			shown: s_
	// 		};
	// 	});
	// }
	
	render_breakdown = (t0, compare = null) => {
		const term_raw = this.state.result.term_raw;
		const ret = []; // // //
		let last_end = 0;
		let i = 0;
		const missed_jsx = (a, b) => <li className="missed-term" key={`${i}m`}>{term_raw.substring(a, b)}</li>;
		const hit_jsx = (t, j, diff = null) =>
			<li className={`hit-term ${diff ? 'diff' : null}`}
			    key={`${i}h`}>
				<span className="hit-unit-wrapper">
				   {this.state.breakdown_type === 'raw'
				   	? null
				   	: { '-1': '/', '1': j > 0 ? <span>&middot;</span> : '' }[t.rpc_sgn]}
					{this.state.breakdown_type === 'raw'
						? term_raw.substring(t.rpc_rng[0], t.rpc_rng[1] + 1)
						: <span>{t.rpc_m_prefix == null ? null : t.rpc_m_prefix.p_sym}{t.rpc_unit.u_sym}</span>
					}
				</span>
				<span className="hit-tooltip">
					<div className="hit-tooltip-wrapper">
						<ul>
							<li key="head" className="unit-head">
								{ t.rpc_sgn === -1 ? <span className="unit-per">per-</span> : '' }
								{ t.rpc_m_prefix == null
									? null
									: <a href={`//en.wikipedia.org/wiki/${t.rpc_m_prefix.p_name}`} className="unit-prefix" target="_blank">
										{t.rpc_m_prefix.p_name}
										(10<sup>{t.rpc_m_prefix.p_fac}</sup>)
									</a>
								}
								<span>&nbsp;</span>
								<a href={`//en.wikipedia.org/wiki/${t.rpc_unit.u_link}`} className="unit-name" target="_blank">
									{t.rpc_unit.u_name}
								</a>
								<span className="unit-abbr">&nbsp; (abbr. <span className="unit-abbr-prefix">{t.rpc_m_prefix ? t.rpc_m_prefix.p_sym : null}</span>{t.rpc_unit.u_sym})</span>
							</li>
							<li key="desc0" className="unit-desc">SI:&nbsp;<span className="unit-unitstr">{t.rpc_unit.u_si.si_fac.toExponential(2)}{this.pprunit(t.rpc_unit.u_si.si_syms)}</span></li>
							<li key="desc1" className="unit-desc">Phrase unit from here to end:&nbsp;<span className="unit-unitstr">{this.pprunit(t.rpc_stash.si_syms)}</span></li>
						</ul>
					</div>
				</span>
			</li>;
		for(const term of t0) {
			if(this.state.breakdown_type === 'raw')
				ret.push(missed_jsx(last_end, term.rpc_rng[0]));
			
			let diff = false;
			if(compare) {
				diff = compare.filter(term_ => term_.rpc_rng[0] === term.rpc_rng[0] && term_.rpc_rng[1] === term.rpc_rng[1]).length === 0;
			}
			ret.push(hit_jsx(term, i, diff));
			last_end = term.rpc_rng[1] + 1;
			i++;
		}
		if(this.state.breakdown_type === 'raw')
			ret.push(missed_jsx(last_end, term_raw.length));
		
		return ret;
	}
	
	render = () =>
		<div>
			<section>
				<form onSubmit={this.onSubmit} id="term_form" ref={this.form_ref}>
					<input type="text" placeholder="Enter a phrase (e.g. mbmbam, Planck yeast)" id="term_raw" name="term_raw" ref={this.term_raw_ref} />
					{ (this.state.n_request > this.state.n_fulfilled) && <div className="lds-ellipsis"><div></div><div></div><div></div><div></div></div> }
					<input type="submit" />
				</form>
			</section>
			{ this.state.result != null
				? null
				: <section id="results_placeholder">
					Results appear after entering a phrase and pressing <kbd>Enter</kbd>
				</section>
			}
			{
				this.state.err == null
					? null
					: <section id="error" className="group">
						<span>Error: {this.state.err[0]}</span>
					</section>
			}
			{
				this.state.result == null || this.state.result.terms[0].length === 0 ? null : <section id="results">
					<h2>This phrase is: <span>{this.state.result.terms[0][0].rpc_stash.si_fac.toExponential(4)}</span> {this.pprunit(this.state.result.terms[0][0].rpc_stash.si_syms)}</h2>
					{this.state.result.nice == null || this.state.result.nice.length === 0
						? null
						: (() => {
							const nice_unit = this.state.result.nice.reduce((a, [b_sgn, b_ut]) => {
								const k = b_ut.ut_name;
								if(!a.hasOwnProperty(k))
									a[k] = 0;
								a[k] += b_sgn;
								return a;
							}, {});
							const nice_unit_arr = Object.entries(nice_unit);
							return <div>
								{ nice_unit_arr.length === 1
										&& Math.abs(nice_unit_arr[0][1]) === 1
										&& nice_unit_arr[0][0] in superlatives
										&&
											(this.state.result.terms[0][0].rpc_stash.si_fac > SUPERLATIVE_LIMS[1]
											|| this.state.result.terms[0][0].rpc_stash.si_fac < SUPERLATIVE_LIMS[0])
									? <div>That's&hellip; {superlatives[nice_unit_arr[0][0]][+(sgn(SUPERLATIVE_LIMS[0] - this.state.result.terms[0][0].rpc_stash.si_fac) === nice_unit_arr[0][1])]}.</div>
									: null }
								<div>(It's a unit of { this.pprunit(nice_unit) })</div>
							</div>
						})()
					}
					<span className="copy-container">
						<input type="text" className="hidden" ref={this.uri_stash_ref} value={window.location.href} onChange={_ => {}} />
						{ this.state.copying ?
							"Copied!" :
							<a href="#" onClick={this.copyURI}>Share this result (copy URI)</a>
						}
					</span>
				</section>
			}
			{ this.state.result == null || this.state.result.terms[0].length === 0 ? null :
				<section id="breakdown">
					<h2>Breakdown:</h2>
					<div id='breakdown_control'>
						<ul id="breakdown_selector">
							<li key="raw">
								<label htmlFor="breakdown_type_raw">Show original string:</label><input type="radio" id="breakdown_type_raw" name="breakdown_type" value="raw" onChange={this.onChangeBreakdownType} checked={this.state.breakdown_type === 'raw'} />
							</li>
							<li key="trunc">
								<label htmlFor="breakdown_type_trunc">Show only units:</label><input type="radio" id="breakdown_type_trunc" name="breakdown_type" value="trunc" onChange={this.onChangeBreakdownType} checked={this.state.breakdown_type === 'trunc'} />
							</li>
						</ul>
					</div>
					<ul className="breakdown-container">{this.render_breakdown(this.state.result.terms[0])}</ul>
					{
						this.state.result.terms.length <= 1
							? null
							: <div id="alternative_container">
								<h3>Alternatives:</h3>
								<ul id="alternative_list">
									{this.state.result.terms.map((t, idx) => 
										idx === 0
											? null
											: <li key={idx}>
												<span className="unit-unitstr">{t[0].rpc_stash.si_fac.toExponential(2)}{this.pprunit(t[0].rpc_stash.si_syms)}</span>
												<ul className="breakdown-container">{this.render_breakdown(t, this.state.result.terms[0])}</ul>
											</li>
									)}
								</ul>
							</div>
					}
				</section>
			}
			
			<section id="about">
				<h2>About</h2>
				<p>
					This tool interprets phrases as if they were a sequence of units, slicing the phrase into unit symbols that cancel out into the smallest unit that covers the most of the phrase. It thereby implements <a href="//xkcd.com/2312" target="_blank">XKCD 2312</a>&dagger;. The engine inserts <a href="//physics.nist.gov/cuu/Units/prefixes.html" target="_blank">SI prefixes</a> where they make the resulting unit smaller. It uses unit symbols, common names, and conversions to SI <a href="//en.wikipedia.org/wiki/Module:Convert/documentation/conversion_data">provided by Wikipedia</a>, specifically <a href="//en.wikipedia.org/wiki/Module:Convert/data">this Lua table</a> that is generated from that page.
				</p>
				<p>
					The engine is written in Haskell and is made up of two parts:
				</p>
				<ol>
					<li>A dynamic-programming algorithm finds the unit that bridges the current string position to a later one with the smallest resulting SI unit, and</li>
					<li>A heuristic <a href="//en.wikipedia.org/wiki/Knapsack_problem">knapsack-problem solver</a> converts the final SI unit back to a more familiar worded form (e.g. m/s &rarr; speed).</li>
				</ol>
				<p>
					Since the DP and knapsack solvers aren't optimal, the results aren't always strictly minimal, but they're usually pretty good and small.
				</p>
				<p>
					Source is available <a href="//github.com/acrylic-origami/phrase2unit" target="_blank">on GitHub</a>. Further details on implementation can be found <a href="//lam.io/projects/p2u" target="_blank">on my blag.</a>
				</p>
				<p><sup>&dagger; Just realized this sentence reads like this tool complies with a standard created by XKCD &mdash; one of more than 2000. <a href="https://xkcd.com/927/" target="_blank">Oh god.</a></sup></p>
			</section>
		</div>
}
