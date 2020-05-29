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
	
	onSubmit = e => {
		this.setState(({ n_request }) => ({ n_request: n_request + 1 }));
		e.preventDefault();
	}
	
	pprunit = u => <ul className="unit-list">
			{u.fac.toExponential(4)}
			&nbsp;
			{
				(() => {
					const units = u.units;
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
				(this.state.result == null || this.state.result.end == null) ? null : <section>
					<h2>This is a[n]:</h2>
					<div className="unit-container">
						<span className="unit-sys-label">SI:</span>
						{this.pprunit(this.state.result.end.si)}
					</div>
					{/*<p>
						<span>Minimal:</span>
						<span>
							{this.pprunit(this.state.result.end.nice)}
							<div>(it's a {this.state.result.end.nice.utype})</div>
						</span>
					</p>*/}
				</section>
			}
			{ this.state.result == null ? null :
				<section id="breakdown_sec">
					<h2>Breakdown:</h2>
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
									{term_raw.substring(t.rng[0], t.rng[1])}
									{ this.state.shown[j]
										? <span className="hit-tooltip">
												<ul>
													<li key="unit-head"><span className="unit-name">{t.name}</span>&nbsp;<span className="unit-utype">({t.utype})</span></li>
													<li key="unit-desc">This unit: <span className="unit-unitstr">{this.pprunit(t) /* TODO WRONG UNIT: need to reference from units.json */}</span></li>
													<li key="unit-desc">Phrase up to here: <span className="unit-unitstr">{this.pprunit(t) /* TODO WRONG UNIT: need to reference from units.json */}</span></li>
												</ul>
											</span>
										: null }
								</li>;
							if(this.state.result.terms != null) {
								for(const term of this.state.result.terms) {
									ret.push(missed_jsx(last_end, term.rng[0]));
									ret.push(hit_jsx(term, i));
									last_end = term.rng[1];
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