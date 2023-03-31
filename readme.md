
# Overview of analysis

This `readme` file describes our analysis of the productivity of CJEU judges. Our analysis is based on our recent study in the Journal of European Public Policy (JEPP):

> Joshua C. Fjelstul, Matthew Gabel & Clifford J. Carrubba (2022). "The timely administration of justice: using computational simulations to evaluate institutional reforms at the CJEU," Journal of European Public Policy, DOI: 10.1080/13501763.2022.2113115.

## Sample

We use data from the `judges` and `judgments` tables of the IUROPA CJEU Database Platform. Our unit of observation is a judgment. Our sample includes every judgment published from 1 May, 2004 (the date of the enlargement into Central and Eastern Europe) through 31 December, 2022. Some of our control variables are based on `list_cited_documents`,  `list_directory_codes`, and `list_subject_keywords`, which are based on EUR-Lex metadata. This metadata is sometimes missing, and our sample only includes judgments where these variables are not missing. There are a small number of judgments where there are multiple Judge-Rapporteurs or the Judge-Rapporteur is unknown. We drop these observations.

## Measurement

The dependent variable in our analysis is the duration of the case in days, `duration_days`. This is the time from the date the case is lodged in the registry to the date that the CJEU publishes the judgment.

The Article 255 panel takes into account the formation of the Court and the subject matter of the case. We control for the formation of the Court using `count_judges`, which is the number of judges on the panel that hears the case. We control for the subject matter of the case using a set of dummy variables. These subject matter dummies are not mutually exclusive (e.g., a judgment can fall into multiple policy areas). The Court uses case law directory codes to classify cases by subject matter, and we use these directory codes as the basis for our subject matter dummies.

The IUROPA CJEU Database Platform includes data on the directory codes assigned to each case for judgments that appear in EUR-Lex. We map each case law directory code to a set of policy areas. The Court changed the case law directory codes after the Treaty of Lisbon, so we do this for both versions.

These policy areas generally correspond to the Directorates-General (DGs) at the Commission. Some DGs have been split and merged over time. We code policy areas based on the least common denominator. We based our policy areas on the organization of DGs because this is how the EU institutions define policy areas internally.

In the Court of Justice model, we include dummy variables for `16` policy areas: `agriculture`, `budget`, `competition`, `consumer_protection`, `customs_union`, `economic_policy`, `environment`, `external_relations`, `home_affairs`, `industrial_policy`, `internal_market`, `justice`, `social_policy`, `taxation`, `trade`, and `transport`.

In the General Court model, we only include dummy variables for the policy areas that are relevant to General Court cases: `agriculture`, `competition`, `industrial_policy`, `internal_market`, and `trade`.

We also include a set of variables that control for case complexity. We include controls that tap into three distinct dimensions of complexity: procedural complexity, legal complexity, and policy complexity.

### Procedural complexity

We use `6` variables to measure procedural complexity: `is_appeal`, `is_direct_action`, `count_joined_proceedings`, `count_opinions`, `count_legal_procedures`, and `count_judges`.

`is_appeal` and `is_direct_action` control for the legal procedure. The omitted category at the Court of Justice, during the time period we analyze, is preliminary rulings. The omitted category at the General Court is staff cases. At the Court of Justice, we expect appeals and direct actions to take longer that preliminary rulings, which the Court prioritizes. 

`count_joined_proceedings` is the number of proceedings that have been joined together. We might expect joined proceedings to take longer because the court has to evaluate the case facts for each proceeding when deciding whether the individual proceedings are similar enough to join and when discussing the proceedings in the judgment. `count_opinions` is the number of Advocate General (AG) opinions in the case. This variable only applies to the Court of Justice. Recently, the court has used its procedural discretion to reduce the number of AG opinions in order to speed up the judicial process. 

At the General Court, we use `count_legal_procedures` to capture the number of distinct legal procedures in a case. This includes major procedures (e.g., actions for annulment, actions for damages, etc.) and minor procedures (e.g., measure of inquiry, applications to intervene, etc.). At the Court of Justice, during the period we analyze, cases only have a single legal procedure. 

We can also think of `count_judges`, which captures the formation of the Court, as a measure of procedural complexity.

### Legal complexity

We use `5` variables to measure legal complexity: `count_cited_documents`, `count_sources_of_law`, `principles_of_law`, `national_law`, and `legal_order`. 

We expect that cases are more legally complex, on average, when the judgment cites more legal documents and when the judgment cites more different sources of law. This requires more analysis and synthesis by the judges, which should increase case duration. `count_cited_documents` is the number of different documents that a judgment cites and `count_sources_of_law` is the number of different sources of law (e.g., primary law, secondary law, and case law) that a judgment cites. 

The `count_cited_documents` variable is based on EUR-Lex citation metadata, and is only available for judgments that appear in EUR-Lex. To calculate `count_sources_of_law`, we use the `list_cited_documents` in the `judgments` table of the IUROPA CJEU Database Platform, which lists the CELEX numbers of all legal documents cited in each judgment. We code the type of law (primary law, secondary law, or case law) based on the CELEX number and then count the number of types of law that each judgment cites. 

`principles_of_law` is a dummy variable that is coded `1` for cases that deal with general principles of law. Case that deal with general principles of law, which are not specific to member states' legal systems, should be simpler, on average, than cases that deal with more specific legal principles. We code a `1` for cases where the court has assigned the `A-01.02` (pre-Lisbon) or `1.01.02` (post-Lisbon) directory codes. 

`national_law` is a dummy variable that is coded `1` for cases that deal with the relationship between national law and EU law. These cases tend to be more political and cases that have broader implications for the development of EU law. We code a `1` for cases where the court has assigned the `A-02, A-03` (pre-Lisbon) or `1.03, 1.05` (post-Lisbon) directory codes.

`legal_order` is a dummy variable that is coded `1` for cases that deal with the legal order of the EU. According to the case law directory, this topic covers the sources of EU law, the values and objectives of the EU, the respective powers of the EU and member states, fundamental rights, the interpretation of EU law, democratic principles, and non-discrimination and citizenship. We expect cases that deal with the legal order of the EU to be more legally complex because they can have broader implications for the functioning of the EU legal system. We code a `1` for cases where the court has assigned directory codes in the `A-##` (pre-Lisbon) or `1.##` (post-Lisbon) sections of the directory.

### Policy complexity

We use `2` variables to measure policy complexity: `count_subject_keywords`, and `count_policy_areas`. We use the number of policy areas, `count_policy_areas`, and the number of keywords that EUR-Lex assigns to the case, `count_subject_keywords`, as proxies for the number of policy areas involved. `count_subject_keywords` is based on `list_subject_keywords` and `count_policy_areas` is based on the subject matter dummies.

## Model specification

We estimate an OLS model for each court. The specifications of these two models are similar, but there are some minor differences. Some variables are omitted from one model or the other due to lack of variation. 

The dependent variable is `duration_days`. Both models include court-specific Judge-Rapporteur fixed effects. The Judge-Rapporteur is the judge who is responsible for managing the case and who writes the draft of the judgment. The base category for the Judge-Rapporteur fixed effects is the current president of each court. The coefficients on the Judge-Rapporteur fixed effects capture the relative productivity of judges, controlling for case characteristics, and are the primary quantities of interest in our analysis. 

We control for the two factors that the Article 255 Panel account for: formation and subject matter. Then, we include a set of variables to control for case complexity.

```
# Model formula
cj_f <- duration_days ~

  # Procedural complexity
  count_judges + count_joined_proceedings + count_opinions +
  is_appeal + is_direct_action +

  # Legal complexity
  count_cited_documents +
  principles_of_law + national_law + legal_order +
  count_sources_of_law +

  # Policy complexity
  count_subject_keywords + count_policy_areas +

  # Subject matter
  agriculture + budget + competition + consumer_protection +
  customs_union + economic_policy + environment + external_relations +
  home_affairs + industrial_policy + internal_market + justice +
  social_policy + taxation + trade + transport +

  # Judge-Rapporteur
  judge_rapporteur
  
# Model formula
gc_f <- duration_days ~

  # Procedural complexity
  count_judges + count_joined_proceedings + count_procedures +
  is_appeal + is_direct_action +

  # Legal complexity
  count_cited_documents +
  principles_of_law + legal_order +
  count_sources_of_law +

  # Policy complexity
  count_subject_keywords + count_policy_areas +

  # Subject matter
  agriculture + competition +
  industrial_policy + internal_market + trade +

  # Judge-Rapporteur
  judge_rapporteur
```

## Productivity index

We create a productivity index for each court based on the estimates of our models. We calculate an index value for each judge at each court. Judges who have served on both courts in the time period we analyze appear have separate values at each court, which are not directly comparable. Our index is based on the estimated coefficients on the Judge-Rapporteur fixed effects. The base category is the current president of each Court, so each coefficient indicates how more or less productive each Judge-Rapporteur is, on average, compared to the president. Higher coefficients indicate less productive judges, and vice versa. We rescale these coefficients to create an index that varies from 0 to 10, where `0` is the value for least productive judge (the judge with the largest coefficient) and `10` is the value for most productive judge (the judge with the smallest coefficient). 

We can use our productivity index to directly compare judges at the same court, but we cannot directly compare judges across courts, as the scaling is different. 
