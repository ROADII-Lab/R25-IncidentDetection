import { useEffect } from "react";
import { Routes, Route } from "react-router-dom";
import ReactGA from "react-ga4";

import Homepage from "./pages/homepage";
import Datasets from "./pages/datasets";
import Information from "./pages/information";
import Demos from "./pages/demos";



import { TRACKING_ID } from "./data/tracking";
import "./app.css";

function App() {
	useEffect(() => {
		if (TRACKING_ID !== "") {
			ReactGA.initialize(TRACKING_ID);
		}
	}, []);

	return (
		<div className="App">
			<Routes>
				<Route path="/" element={<Homepage />} />
				<Route path="/datasets" element={<Datasets />} />
				<Route path="/info" element={<Information />} />
				<Route path="/demos" element={<Demos />} />
			</Routes>
		</div>
	);
}

export default App;
