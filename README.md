## **Project Background**

Conjoint analysis is a popular method used in marketing and product research to understand how consumers make trade-offs between different features when selecting a product. 
Rather than asking users directly what they value most, conjoint analysis infers the value ("utility") of each product feature by observing their choice behavior, given a set of options, under an actual or hypothetical setup.

This approach is highly relevant in competitive industries like streaming, where product packages are composed of multiple attributes. 
In the dataset example, the features included number of accounts, additional content, ad options, and price.
Each of the feature/attribute then has mutually-exclusive list that typically is called levels; for example, under additional content, we have Disney, HBO, Soccer, etc.  

Conjoint analysis can help businesses answer:
- Which features influence customer decisions most?
- How much are users willing to pay for premium options?
- What bundle of features maximizes uptake or revenue?

This project applies conjoint analysis to simulate Netflix-style product configurations and predict how different combinations of features and pricing would perform in the market. 


## Data & Modeling Pipeline

- **Data Source**: Public dataset from GitHub containing user choices across multiple Netflix package profiles
- **Preprocessing & Exploration**:
  - Transformed few attributes into factor
  - Did a experimental design checking: figured out that each respondent could be exposed to a different number of options and select multiple, unlike the typical forced-choice CBC
- **Modeling**:
  - Trained a binomial logistic regression (`glm`) to estimate utility per feature level, with `selected = 1/0` as outcome variable
  - Calculated WTP = utility / (- price coefficient)
  - Simulated Share of Preference (SoP) using softmax transformation of predicted utilities
- **Conjoint Results Visualization & Simulation**:
  - Visualized the utilities and significance of levels
  - Simulated few hypothetical product configurations scenario & price sensitivity - how each product profile SoP will change given different price points

### Modeling Choice Justification

In a typical choice-based conjoint (CBC) setup, respondents choose one option from a fixed set per task, allowing the use of multinomial logit (MNL) to predict the winner within a fixed set.

However, in this dataset:
- Each respondent is exposed to a variable number of product options (5 to 12)
- They are free to select any number of options (some only selects 1 option, others select up to 6 options at a time)

Because there is no fixed choice set or mutually exclusive selection, I therefore used binomial GLM to predict the likelihood of selection per row/product profile, 
treating each row as a binary outcome (`selected = yes 1 / no 0`).


## Key Outputs

- R script, consisting of the data preprocessing, modeling, and simulation & visualization of the results (utilities, share of preference and price sensitivity curves)
- Interactive R Shiny dashboard for configuring Netflix-like packages _--> **still on progress**_


## Limitations & Future Improvements

- Assumes monopoly (only Netflix) in the share of preference estimates — no competitor products considered
- Current model assumes **additive and linear utility**; no interactions are modeled
- The GLM only estimates aggregate-level utilities whereby it pools all respondents together in estimating the utilities.
  It does not provide individual-level predictions, let alone further segment users or estimate heterogeneity (such as via HB or mixed logit)
- Price is treated as linear; real pricing sensitivity may be non-linear
- Future work could include:
  - Researching and using different, more sophisticated models relevant for the use case
  - Depending on the models' ability, provide individual-level utility predictions
  - Competitor simulation & adding customer segmentation data (this should be done on the research design level!), to provide a more robust and realistic worldview
  - Interface upgrades to show significance, feature importance, or user segments


##
_Note: This project is built as an educational exercise to demonstrate the application of conjoint analysis for simulation and decision-making. 
While care has been taken in modeling and interpretation, assumptions may limit real-world generalizability. 
Feedback and suggestions are very welcome! And if you find this useful or want to collaborate, feel free to connect!_

