import React from "react";
import { Link } from "react-router-dom";

import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faChevronRight } from "@fortawesome/free-solid-svg-icons";

import "./styles/dataset.css";

const Dataset = (props) => {
	const { title, description, date, link } = props;

	return (
		<React.Fragment>
			<div className="homepage-dataset">
				<div className="homepage-dataset-content">
					<div className="homepage-dataset-title">{title}</div>
					<div className="homepage-dataset-description">
						{description}
					</div>
					<div className="homepage-dataset-link">
						<Link to={link}>
							Go to dataset{" "}
							<FontAwesomeIcon
								style={{ fontSize: "10px" }}
								icon={faChevronRight}
							/>
						</Link>
					</div>
				</div>
			</div>
		</React.Fragment>
	);
};

export default Dataset;
