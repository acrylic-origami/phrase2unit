import React from 'react';
import Q from 'q';

export default class extends React.Component {
	constructor(props) {
		super(props);
		this.form_ref = React.createRef();
		this.state = {
			result: null,
			shown: [], // a little weird to first-class this
			n_request: 0,
			n_fulfilled: 0,
			breakdown_type: 'raw',
			request: null,
			err: null,
			term_raw: 'as the moon shines'
		};
	}
	
	componentDidMount(_, l) {
		// if(l.n_request != this.state.n_request) {
			const n_request_stash = this.state.n_request
			const term_raw = this.state.search;
			const fail = e => {
				console.log(e);
				this.setState(s => (n_request_stash === s.n_request) && { n_fulfilled: n_request_stash });
			}
			const P = fetch(`/end/q`, { method: 'POST', body: new FormData(this.form_ref.current) })
				.then((res) => {
					if(res.ok)
						return res.json();
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
		// }
	}
	
	onTermChange = e => this.setState({ term_raw: e.target.value });
	
	onChangeBreakdownType = e => this.setState({ breakdown_type: e.target.value });
	
	onSubmit = e => {
		this.setState(({ n_request }) => ({ n_request: n_request + 1 }));
		e.preventDefault();
	}
	
	pprunit = u => <ul className="unit-list">
			{
				(() => {
					const units = Object.entries(u);
					units.sort((a, b) => a[1] < b[1]);
					return units.map(([name, power], idx) => 
						<li key={idx}>{power < 0 ? '/' : (idx > 0 && (<span>&middot;</span>))}{name}<sup>{Math.abs(power) > 1 && Math.abs(power)}</sup></li>
					);
				})()
			}
		</ul>
		
	term_mouse = (e, i) => {
		const e_type = e.type; // grr event pooling
		this.setState(s => {
			const s_ = s.shown.slice();
			s_[i] = e_type === 'mouseenter';
			return {
				shown: s_
			};
		});
	}
	
	render = () =>
		<div>
			<section>
				<form onSubmit={this.onSubmit} ref={this.form_ref}>
					<input type="text" value={this.state.term_raw} onChange={this.onSearchChange} name="term_raw" />
					<input type="submit" />
				</form>
			</section>
			{
				this.state.result == null ? null : <section>
					<h2>This is a[n]:</h2>
					<div>
						{this.pprunit(this.state.result.nice.reduce((a, [b_sgn, b_ut]) => {
							const k = b_ut.ut_name;
							if(!a.hasOwnProperty(k))
								a[k] = 0;
							a[k] += b_sgn;
							return a;
						}, {}))}
					</div>
					<div className="unit-container">
						<span className="unit-sys-label">SI:&nbsp;</span>
						<span>
							<span>{this.state.result.terms[0].rpc_stash.si_fac.toExponential(4)}</span>
							{this.pprunit(this.state.result.terms[0].rpc_stash.si_syms)
							/* Object.entries(this.state.result.terms[0].rpc_stash.si_syms)
								.map(a => [a[0], a[1] * this.state.result.terms[0].rpc_sgn]) */}
						</span>
					</div>
				</section>
			}
			{ this.state.result == null ? null :
				<section id="breakdown_sec">
					<h2>Breakdown:</h2>
					<div className='breakdown-control'>
						<ul>
							<li key="raw">
								<label htmlFor="breakdown_type_raw">Show original string:</label><input type="radio" id="breakdown_type_raw" name="breakdown_type" value="raw" onChange={this.onChangeBreakdownType} checked={this.state.breakdown_type === 'raw'} />
							</li>
							<li key="trunc">
								<label htmlFor="breakdown_type_trunc">Show only units:</label><input type="radio" id="breakdown_type_trunc" name="breakdown_type" value="trunc" onChange={this.onChangeBreakdownType} checked={this.state.breakdown_type === 'trunc'} />
							</li>
						</ul>
					</div>
					<ul id="breakdown_container">{
						(() => {
							const term_raw = this.state.result.term_raw;
							const ret = []; // // //
							let last_end = 0;
							let i = 0;
							const missed_jsx = (a, b) => <li className="missed-term" key={`${i}m`}>{term_raw.substring(a, b)}</li>;
							const hit_jsx = (t, j) =>
								<li className="hit-term"
								      key={`${i}h`}
								      onMouseEnter={e => this.term_mouse(e, j)}
								      onMouseLeave={e => this.term_mouse(e, j)}>
								   {this.state.breakdown_type === 'raw'
								   	? null
								   	: { '-1': '/', '1': <span>&middot;</span> }[t.rpc_sgn]}
									{term_raw.substring(t.rpc_rng[0], t.rpc_rng[1] + 1)}
									{ this.state.shown[j]
										? <span className="hit-tooltip">
												<ul>
													<li key="head" className="unit-head">
														{ t.rpc_m_prefix == null ? null : <a href={`//en.wikipedia.org/wiki/${t.rpc_m_prefix.p_name}`} className="unit-prefix" target="_blank">{t.rpc_m_prefix.p_name} (10<sup>{t.rpc_m_prefix.p_fac}</sup>)</a> }
														<a href={`//en.wikipedia.org/wiki/${t.rpc_unit.u_link}`} className="unit-name" target="_blank">
															{t.rpc_unit.u_name}
														</a>
														<span className="unit-abbr">&nbsp; (abbr. <span className="unit-abbr-prefix">{t.rpc_m_prefix ? t.rpc_m_prefix.p_sym : null}</span>{t.rpc_unit.u_sym})</span>
													</li>
													<li key="desc0" className="unit-desc">SI:&nbsp;<span className="unit-unitstr">{this.pprunit(t.rpc_unit.u_si.si_syms)}</span></li>
													<li key="desc1" className="unit-desc">Phrase unit up to here:&nbsp;<span className="unit-unitstr">{this.pprunit(t.rpc_stash.si_syms)}</span></li>
												</ul>
											</span>
										: null }
								</li>;
							if(this.state.result.terms != null) {
								for(const term of this.state.result.terms) {
									if(this.state.breakdown_type === 'raw')
										ret.push(missed_jsx(last_end, term.rpc_rng[0]));
									
									ret.push(hit_jsx(term, i));
									last_end = term.rpc_rng[1] + 1;
									i++;
								}
							}
							ret.push(term_raw.substring(last_end, term_raw.length));
							return ret;
						})()
					}</ul>
				</section>
			}
		</div>
}