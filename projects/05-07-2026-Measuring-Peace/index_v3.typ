// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

#show: doc => article(
  title: [Quantitative measures of peacebuilding interventions],
  authors: (
    ( name: [Erik H. Knudsen],
      affiliation: [],
      email: [] ),
    ),
  date: [2026-03-29],
  toc: true,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Measuring peace impact
<measuring-peace-impact>
Conducting impact evaluations of peacebuilding projects are a notoriously hard challenge due to the multidimensional nature of peace and the multiple conceptions of the term @davenportContemporaryStudiesPeace2018, which from a applied reserach and evaluation perspective creates certin challanges when it comes to measuring peace as an outome in impact evaluations. Recently, I, together with colleagues at NCG, finished a series of peacebuilding impact evaluations, where we had to deal with the above challanges. Therefore, I will in this blog post describe how we dealt with these challanges, focusing especially on the application of open source quantitative data sources.

Firslty, as in any other evaluation, it is important that we in the initial phase of the evaluation are conscious about our definition of peace, since it conditions our choice of data sources. As mentioned, peace is a multidimensional concept, which is best viewed as an continuum, ranging from #emph[negative peace] (the absence of personal violence) to #emph[positive peace] (the absence of structural violence) @galtungViolencePeacePeace1969@davenportContemporaryStudiesPeace2018. Which end of this continuum we find relevant determines what we can count as evidence of success, and therefore the kind of data we would need to look for. Moreover, the continuum also entails that we approach peace from different angles, and thus utlise different data sources.

An important tool to help us resolve this definitional question is to apply a theory-based evaluation (TBE) approach which lets the evaluated project's own programme logic decide what to focus on. The core move is to reconstruct the project's theory of change, i.e., the hypothesised chain of #emph[if X then Y] assumptions linking the project's activities and contextual variables to the higher-level peace outcomes it claims to pursue. @sternBroadeningRangeDesigns2012. The approach therefor aids us in developing an #emph[operational] definition of peace. Meaning, it becomes explicit which dimension of peace the project is targeting, and in doing so it tells us what to measure and where to look for it. On a related note, adopting a TBE lens is particularly valuable when conducting evaluations of peacebuilding projects because the conditions for a credible counterfactual rarely exist in conflict settings, and peace processes are complex, non-linear and long-term. It therefore shifts the central question from #emph[attribution];, did the project #emph[produce] peace?, to #emph[contribution];, did it plausibly #emph[contribute] to change along the hypothesised pathway, while avoiding harm?

An important guiding tool when locating potential data soures for our measure is to deal with our understnaing of what defines impact, which in the common OECD-DAC sense entails "#emph[the extent to which the intervention has generated or is expected to generate significant positive or negative, intended or unintended, higher-level effects];" @oecdApplyingEvaluationCriteria2021. This defintion demands us to move away from output-based measures and towards the more general, outcome-level measures sitting further along the causal chain. Encapsulating these "#emph[higher level effects];" will in most cases require us to apply a mix of both qualitative and quantitative sources, but is also important, since it allows us to triangulate our findings, and thereby the validity of the results.

= Examples used
<examples-used>
The examples in this post will build on data and evaluation questions, which firstly were part of NCG's mandate to undertake a series of impact evaluations of the Swedish International Development Cooperation Agency's (Sida) work with poverty. In connection with this evaluation I, together with Anne-Lise Klausen, conducted an #link("https://cdn.sida.se/app/uploads/2026/01/27120639/62844_EVA2026_1h_Ev-of-the-Nexus-Pilot-in-South-Sudan_webb-1.pdf")[impact analysis of the "Nexus Pilot in Budi County" project in the Eastern Equatoria State in South Sudan];. The main aim of the project being evaluated is to strengthen #emph[peaceful and inclusive societies for sustainable development through strengthened community resilience, enhanced social cohesion and transformed socio-economic well-being];. In order to capture the impact of the project, it was decided to conduct extensive field interviews with key stakeholders within the villages of Budi and Ikotos counties. Secondly, I will draw on examples from the NCG-led evaluation of the Swiss Agency for Development and Cooperation's (SDC) engagement in the area of peacebuilding (2019-2024), which covered several country cases using a mix of qualitative and quantitative data sources. Thirdly, I will draw on my own analyses and examples from the academic literature.

= Observed levels of violence using ACLED or UCDP data
<observed-levels-of-violence-using-acled-or-ucdp-data>
A recurring claim in the field interviews as part of the evaluation of the "Nexus Pilot in Budi County" was that the project had resulted in decreased levels of violence. However, due to an inability to visit all project locations and to possible social desirability bias (described in the main report), we wanted to use another data source to validate and generalise this finding. Here we settled on using the Armed Conflict Location and Event Data (ACLED), which is a comprehensive dataset covering conflict events around the world. Another slightly similar dataset is the Uppsala Conflict Data Program (UCDP) produced by Uppsala University and, contrary to ACLED, is free. Both data sources allow us with a highe degree of spatial graniluraity to investigate the development in levels of violent acts before and after the initiation of the project. However, compared to UCDP, ACLED includes a broader set of violence-related events, including non-lethal ones. For this analysis I will restrict the analysis to events categorised as "Battles" and "Violence against civilians", since it is the main violence that the project sought to reduce. Trends in both the number of events and the number of fatalities are depicted in figure 1 below. The identified trend in both figures of increasing levels of violence in the project's intervention period runs counter to the collected perception of decresed levels of violence. Concretely, we see an increase in the number of violent events in 2021 (one year after the project started), which does not decrease in the following years. Moreover, the number of fatalities also increased in 2021; however, it decreases in the following years, but not below the pre-intervention levels. An explanation of this apparent contradiction is that the two sources speak to different levels of effect. The interview claim reflects a local, and possibly idiosyncratic, perception of safety in respondents' immediate surroundings, whereas the ACLED trend captures the broader development in violence across the project area.

#block[
```r
library(tidyverse)
library(readr)
library(tidytext)
library(topicmodels)
library(tm)
library(topicdoc)
library(RColorBrewer)
library(gghighlight)
```

]
#block[
```r
#############
### ACLED violence
#############

# ACLED data covering South Sudan (download from https://acleddata.com; requires registration)
acled_path <- "path/to/2016-01-01-2024-12-31-South_Sudan.csv"

ACLED <- read_csv(acled_path)

focus_df <- ACLED |>
  filter(admin2 %in% c("Ikotos", "Budi")) |>
  filter(event_type %in% c("Battles", "Violence against civilians")) |>
  filter(sub_event_type != "Non-state actor overtakes territory")
```

]
#block[
```r
#############
### ACLED violence — number of events and fatalities per year
#############

# `focus_df` is created in the acled-load-filter chunk above

n_events <- focus_df |> 
  group_by(admin2, year) |> 
  count()

fatalities <- focus_df |> 
  group_by(admin2, year) |> 
  summarise(fatal = sum(fatalities))


combined_df <- cbind(n_events, fatalities[,"fatal"])


# Reshape the data to long format
combined_long <- combined_df |>
  pivot_longer(
    cols = c(n, fatal),
    names_to = "type",
    values_to = "value"
  ) |>
  mutate(
    type = recode(
      type,
      fatal = "Number of Fatalities",
      n = "Number of violent battles and attacks on civilians"
    )
  )


# Define custom colors for each admin2
custom_colors <- c(
  "Budi" = "#16b7e8ff",   # Budi (teal)
  "Ikotos" = "#33e7f4df"  # Ikotos (light teal)
)

library(ggh4x)

vio_plot <- ggplot(combined_long, aes(x = factor(year), y = value, fill = admin2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid2(. ~ type, scales = "free_y", independent = "y") +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(
    x = "Year",
    y = "",
    fill = "County",
    caption = "Source: ACLED"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18),  # Increase title font size
    plot.subtitle = element_text(size = 16),  # Increase subtitle font size
    axis.title.x = element_text(size = 16),  # Increase x-axis title font size
    axis.title.y = element_text(size = 16),  # Increase y-axis title font size
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase x-axis text font size
    axis.text.y = element_text(size = 14),  # Increase y-axis text font size
    legend.text = element_text(size = 14),  # Increase legend text font size
    strip.text = element_text(size = 11),  # Increase facet label font size
    legend.title = element_text(size = 16),  # Increase legend title font size
    panel.grid.major = element_line(color = "grey80", linetype = "solid"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
    legend.position = "top",
    plot.caption = element_text(size = 14, face = "italic"),  # Increase caption font size
    panel.spacing = unit(3, "lines")  # Add space between facets
  ) + 
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 7.5, linetype = "dashed", color = "black", size = 1) +
  geom_text(aes(x = 6, y = -2, label = "Project Period"), vjust = 1, hjust = 0.5, size = 5, color = "black")
```

]
#block[
#block[
#set text(weight: "bold"); Figure 1 - Number of violent events and fatalities by year
]
#box(image("figures/figure_2_faceted_plot.png"))

]
= Semantic analysis of patterns of violence
<semantic-analysis-of-patterns-of-violence>
Another important data source when investigating peacebuilding interventions is text data, which is also possible using ACLED, as the data source contains a short description of the recorded event, allowing us to investigate specific flavours of violent events. This is an approach relevant for the Budi evaluation, since its theory of change focused on reducing cattle raiding and tensions between ethnic groups. In order not to manually go through all the recorded cases of violence to investigate whether they relate to the aforementioned topics, I will use natural language processing (NLP). It is important to mention that this specific source of text data is descriptive, and therefore I will apply tools that capture attributes or nuances related to a violent event; however, if the text data instead came from a more normative source, where each data point relates to a normative statement about an ethnic group --- which would be the case for social media posts --- we could use NLP combined with machine learning models or large language model methods to investigate whether levels of hate speech had changed after the end of the project. I will link to articles on the aforementioned matter at the end of the post, but here I will showcase examples using ACLED.

I begin by applying a series of text preprocessing steps to each event description, which involves lower-casing all words, removing redundant terms like URLs, punctuation, and numbers, and reducing each word to its root form using lemmatization. This is an important step, as the analysis would otherwise be cluttered by non-important information (you can investigate each step in the code below).

#block[
```r
# Cleaning text
acled_text <- focus_df |> 
  select(notes, year, event_type, sub_event_type, actor1, actor2) |> 
  # Keep only rows with non-missing, non-empty notes
  filter(!is.na(notes), str_trim(notes) != "") |> 
  mutate(
    doc_id = row_number(),
    # Lowercase
    notes = str_to_lower(notes),

    # Remove URLs
    notes = str_remove_all(notes, "https?://\\S+|www\\.\\S+"),

    # Remove email addresses
    notes = str_remove_all(notes, "\\S+@\\S+\\.\\S+"),

    # Remove numbers (standalone digits; keep alphanumeric if needed)
    notes = str_remove_all(notes, "\\b\\d+\\b"),

    # Remove punctuation (keep hyphens between words)
    notes = str_replace_all(notes, "[^a-z\\s\\-]", " "),

    # Collapse multiple spaces
    notes = str_squish(notes)
  )

write_csv(acled_text, file = "projects/29-03-2026-South-Sudan-Violence/data/acled_textR.csv")
```

]
#block[
```python
# Loading libaries
import pandas as pd
import spacy
import re
from spacy.lang.en.stop_words import STOP_WORDS

# Loading spacy language model 
nlp_lem = spacy.load("en_core_web_sm", disable=["parser", "ner"])

# Loading data
acled_text = pd.read_csv("projects/29-03-2026-South-Sudan-Violence/data/acled_textR.csv")

# Performing lemmatization
lem_list =[]
for text in acled_text["notes"]:
  doc = nlp_lem(text)
  tokens = [token.lemma_ for token in doc if not token.is_stop and not token.is_punct and not token.is_space and token.lemma_.strip() != ""]
  new_text =  " ".join(tokens)
  lem_list.append(new_text)


# Adding list to original data frame 
acled_text["lem_notes"] = lem_list

# Saving df as csv
acled_text.to_csv("projects/29-03-2026-South-Sudan-Violence/data/acled_textLEM.csv", index=False)
```

]
#block[
```r
# Loading the new lemmatized text
acled_text <- read_csv("projects/29-03-2026-South-Sudan-Violence/data/acled_textLEM.csv")

# Looking at the preproccessing of the text
summary(map_int(acled_text$lem_notes, \(x) nchar(x))) # Density of characters in the lemmatized text

# Create a corpus from the lemmatized text
corpus <- Corpus(VectorSource(acled_text$lem_notes))

# Create a document-term matrix
dtm_raw <- DocumentTermMatrix(corpus, control = list(wordLengths  = c(2, Inf)))

# Term document frequencies
term_doc_freq <- colSums(as.matrix(dtm_raw > 0))
term_doc_prop <- term_doc_freq / nrow(dtm_raw)

# See the distribution
summary(term_doc_freq)
quantile(term_doc_prop, probs = c(0.01, 0.05, 0.10, 0.50, 0.90, 0.95, 0.99))

# Inspect what I will lose at different thresholds
sort(term_doc_freq[term_doc_freq <= 5])    # what does MIN=5 remove?
sort(term_doc_prop[term_doc_prop >= 0.80], decreasing = TRUE)  # what does MAX=0.80 remove?

# Based on the above analysis I will settle in the following thresholds 
MIN_DOC_FREQ  <- 1    # keep all terms 
MAX_DOC_FREQ  <- 0.80

rm(dtm_raw)

# Creating the final document-term matrix
dtm <- DocumentTermMatrix(corpus,  control = list(
                                                      wordLengths  = c(2, Inf), 
                                                      bounds  = list(global = c(MIN_DOC_FREQ, Inf))))

# Remove very frequent terms (appear in > MAX_DOC_FREQ proportion of docs)
term_freq   <- colSums(as.matrix(dtm) > 0) / nrow(dtm)
keep_terms  <- names(term_freq[term_freq <= MAX_DOC_FREQ])
dtm         <- dtm[, keep_terms]
```

]
The first analytical tool I will use is Latent Dirichlet Allocation (LDA), which is an old NLP technique enabling one to identify topics within a text. Similar to the preprocessing steps, it requires you to make a series of analytical choices. The most important one is to a priori specify the number of topics that the corpus of texts and the individual texts can contain. In the code below I test the effect of each choice and compare them using relevant metrics, and assess the results manually. The best result is achieved using five topics, and importantly topic 3 is assessed to reflect cattle raiding (see figure 2).

#block[
```r
# =============================================================================
# Tune and create final model 
# =============================================================================
### Firstly I will tune the model to find the optimal number of topics

candidate_k <- seq(2, 8, by = 1)

lda_metrics <- map_dfr(candidate_k, function(k) {
  lda_temp <- LDA(dtm, k = k, method = "Gibbs",
                  control = list(seed = 42, iter = 1000))
  
  scores <- topic_coherence(lda_temp, dtm, top_n_tokens = 10)
  
  tibble(
    k              = k,
    perplexity     = perplexity(lda_temp, newdata = dtm),
    mean_coherence = mean(scores),
    min_coherence  = min(scores),
    sd_coherence   = sd(scores)
  )
})

# Pivot to long format for facet_wrap
lda_metrics_long <- lda_metrics |>
  select(k, perplexity, mean_coherence) |>
  pivot_longer(cols = c(perplexity, mean_coherence),
               names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric,
    "perplexity"     = "Perplexity (lower = better fit)",
    "mean_coherence" = "Coherence (higher = more interpretable)"
  ))

ggplot(lda_metrics_long, aes(k, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = candidate_k) +
  labs(title = "LDA Model Selection: Perplexity vs Coherence",
       x = "Number of Topics (k)", y = NULL) +
  theme_minimal()

# Set BEST_K based on my diagnostics above
BEST_K <- 5
 
lda_model <- LDA(
  dtm,
  k       = BEST_K,
  method  = "Gibbs",
  control = list(
    seed    = 42,
    iter    = 4000,       # more iterations for better convergence
    burnin  = 1000,       # proportionally longer burn-in
    thin    = 100,
    alpha   = 1 / BEST_K, # low alpha: documents concentrate on fewer topics
    delta   = 0.01         # low delta: topics concentrate on fewer words
  )
)

### Loading model 
#lda_model <- readRDS("projects/29-03-2026-South-Sudan-Violence/lda_model.rds")

# =============================================================================
# Inspect topics
# =============================================================================

# Top terms per topic (beta matrix)
topic_terms <- tidy(lda_model, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 15) |>
  ungroup() |>
  arrange(topic, -beta)
 
# Print top 15 terms per topic
topic_terms |>
  group_by(topic) |>
  summarise(top_terms = paste(term, collapse = ", ")) |>
  print(n = Inf)
 
# Visualise top terms per topic
lda_top_terms <- topic_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  labs(title = "Top 15 Terms per LDA Topic",
       x = "Term probability (beta)", y = NULL) +
  theme_minimal(base_size = 11)

ggsave("projects/29-03-2026-South-Sudan-Violence/figures/figure_3_lda_top_terms.png", lda_top_terms, width = 14, height = 10)

### Saving the model 
#saveRDS(lda_model, "projects/29-03-2026-South-Sudan-Violence/lda_model.rds")
```

]
#block[
#block[
#set text(weight: "bold"); Figure 2 - Top terms per LDA topic
]
#box(image("figures/figure_3_lda_top_terms.png"))

]
Having settled on the final topic model containing five topics, I will now apply it to the full dataset and calculate the composition of topics for each year. As described in the previous section, I will focus on topic 3. Figure 3 below shows that at the end of the project's period topic 3 peaked, indicating that cattle raiding became more prevalent; however, we see a decrease in the years after. Instead of using topics as an indicator for cattle raiding, in figure 4 I use specific word mentions related to cattle raiding --- the mention of specific words or combinations of them: ((cattle OR cow OR herd OR livestock) AND raid) OR (cattle OR cow OR herd OR livestock). A similar, yet not identical, picture emerges: there is also an increase in cattle raiding across the project period, but not the sharp decrease indicated by the topic model.

#block[
```r
# =============================================================================
# Tmeporal analysis of topic distribution
# =============================================================================

# Assign each event to its most probable topic
dominant_topic <- tidy(lda_model, matrix = "gamma") |>
  mutate(document = as.integer(document)) |>
  group_by(document) |>
  slice_max(gamma, n = 1) |>
  ungroup()

# Join with ACLED data
 acled_topics <- acled_text |>
  left_join(dominant_topic, by = c("doc_id" = "document"))

# Count events per topic per year
topic_by_year <- acled_topics |>
  count(year, topic) |>
  group_by(year) |>
  mutate(pct = n / sum(n) * 100)

n_per_year <- acled_text |>
  count(year, name = "n_events")

# Plot
topic_year <- ggplot(topic_by_year, aes(year, pct, color = factor(topic), alpha = if_else(topic == 3, 1, 0.2))) +
  geom_line(size = 1.5) +  # Increase line thickness
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +  
  scale_alpha_identity() +
  labs(title = "Thematic Composition of Violent Events by Year",
       x = "Year", y = "Share of Events (%)", color = "Topic") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "black", size = 1) +
  geom_text(aes(x = 2020.5, y = -2, label = "Project Period"), vjust = 1, hjust = 0.5, size = 4, color = "black") +
  theme_minimal() 

ggsave("projects/29-03-2026-South-Sudan-Violence/figures/figure_4_topic_year.png", topic_year, width = 14, height = 10)
```

]
#block[
#block[
#set text(weight: "bold"); Figure 3 - Topic focus across years
]
#box(image("figures/figure_4_topic_year.png"))

]
#block[
```r
# NOTE: the detection below currently reduces to the keyword match alone, because
# `(A & B) | A` simplifies to `A`. If you intended raid-specific cattle mentions,
# drop the trailing OR clause; otherwise this matches any cattle/cow/herd/livestock note.
word_plot_year <- acled_text |> 
  group_by(year) |> 
  summarise(
    n_cattle_raid = sum((str_detect(lem_notes, "cattle|cow|herd|livestock") & str_detect(lem_notes, "raid")) | str_detect(lem_notes, "cattle|cow|herd|livestock")),
    n_events      = n(),
    pct           = n_cattle_raid / n_events * 100) |> 
  ggplot(aes(x = year, y = pct)) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "black", size = 1) +
  geom_text(aes(x = 2020.5, y = -2, label = "Project Period"), vjust = 1, hjust = 0.5, size = 4, color = "black") +
  geom_point(color = "#16b7e8ff", size = 3) +
  geom_line(color = "#16b7e8ff", size = 1) + 
  labs(title = "Cattle raid related events",
       x = "Year", y = "Share of Events (%)") +
  theme_minimal() 

ggsave("projects/29-03-2026-South-Sudan-Violence/figures/figure_5_word_year.png", word_plot_year, width = 14, height = 10)
```

]
#block[
#block[
#set text(weight: "bold"); Figure 4 - Share of cattle raiding events
]
#box(image("figures/figure_5_word_year.png"))

]
= Internally displaced people as a measure of indirect violence
<internally-displaced-people-as-a-measure-of-indirect-violence>
Another measure of conflict intensity is to look at the number of internally displaced persons (IDPs) in the area investigated, as recorded by the International Organisation for Migration (IOM)'s Displacement Tracking Matrix. IOM collects data through enumerators who conduct key-informant interviews with persons on site. I use the `dtmapi` package to connect to IOM's API, allowing me to fetch data for the two counties being investigated as part of the evaluation, and plot the number of IDPs over time and the reason for fleeing, as seen in figure 5. Similar to the number of violent events, we also see an increase in the number of IDPs in the two counties during the project period; however, this decreases in the following years. When asked why IDPs have chosen to flee, the reason is, according to key informants' assessments, mostly conflict.

#block[
```r
#############
### IDPs
#############
library(lubridate)
library(scales)
library(stars)
library(sf)
library(dtmapi)
library(patchwork)
library(ggtext)   # for element_markdown() in the caption below

# Define custom colors for each admin2
custom_colors <- c(
  "Budi" = "#16b7e8ff",   # Budi (teal)
  "Ikotos" = "#33e7f4df"  # Ikotos (light teal)
)

# Get data 
idp_SS_df <- get_idp_admin2_data(CountryName='South Sudan', FromReportingDate='2016-01-01', ToReportingDate='2024-12-02')

# Filter data 
idp_SS_df <- idp_SS_df |> 
  filter(assessmentType == "BA") |> 
  filter(admin2Name %in% c("Budi", "Ikotos")) 

# Counting the number of IDPs in each county per year (most recent round per year)
# NOTE: dropped a trailing knitr::kable() here so the result stays a data frame
# that can be passed to ggplot() below.
idp_sum_df <- idp_SS_df |> 
  group_by(yearReportingDate, admin2Name, displacementReason) |> 
  slice_max(roundNumber, n = 1, with_ties = FALSE) |> 
  group_by(yearReportingDate, admin2Name) |> 
  summarise(numPresentIdpInd = sum(numPresentIdpInd, na.rm = TRUE), .groups = "drop")

# Plot: IDP totals per county per year
idps_plot <- ggplot(idp_sum_df,
       aes(x = yearReportingDate, 
           y = numPresentIdpInd, 
           group = admin2Name, 
           color = admin2Name)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma, 
                     breaks = seq(0, max(idp_sum_df$numPresentIdpInd), by = 2500)) +
  scale_x_continuous(breaks = unique(idp_sum_df$yearReportingDate), labels = unique(idp_sum_df$yearReportingDate)) + 
  scale_color_manual(values = custom_colors) +
  labs(x = "",
       y = "Number of IDPs",
       color = "County",
       caption = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80", linetype = "solid"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
    legend.position = "top", 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14),
    plot.caption = element_text(size = 9, face = "italic")
  ) + 
  annotate("rect",
    xmin = 2019, xmax = 2022,
    ymin = -Inf, ymax = Inf,
    alpha = 0.15, fill = "steelblue")


# Aggregate by year 
reason_year_df <- idp_SS_df |>
  group_by(yearReportingDate, admin2Name, displacementReason) |>
  slice_max(roundNumber, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(yearReportingDate, displacementReason) |>
  summarise(numPresentIdpInd = sum(numPresentIdpInd), .groups = "drop") |>
  mutate(displacementReason = case_when(
    str_detect(displacementReason, "No reason") ~ "Unknown / Not recorded",
    TRUE ~ displacementReason
  )) |>
  # Calculate percentage within each year
  group_by(yearReportingDate) |>
  mutate(
    total = sum(numPresentIdpInd),
    pct   = numPresentIdpInd / total * 100
  ) |>
  ungroup()

## Creating response plot
reason_plot <- ggplot(reason_year_df,
       aes(x = yearReportingDate,
           y = pct,
           fill = displacementReason)) +

  annotate("rect",
           xmin = 2019, xmax = 2022,
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "steelblue") +

  geom_col(position = "stack", width = 0.7, alpha = 0.9) +

  # Optional: add percentage labels inside the bars (only if segment is big enough)
  geom_text(aes(label = ifelse(pct > 8, paste0(round(pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +

  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = seq(0, 100, by = 25),
                     limits = c(0, 100),
                     expand = expansion(mult = c(0.02, 0.03))) +
  scale_x_continuous(breaks = unique(reason_year_df$yearReportingDate),
                     expand = expansion(mult = 0.05)) +
  scale_fill_brewer(palette = "Set2") +

  labs(x    = NULL,
       y    = "Share of IDPs (%)",
       fill = "Displacement reason") +

  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, color = "grey30"),
    axis.text.y        = element_text(color = "grey30"),
    axis.title.y       = element_text(margin = margin(r = 10), color = "grey20"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    legend.title       = element_text(size = 11),
    plot.margin        = margin(10, 15, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 2))

# Combing plots
combined_plot <- idps_plot / reason_plot +
  plot_layout(heights = c(2, 1)) +   # give line plot slightly more space
  plot_annotation(
    title    = "",
    subtitle = "",
    caption  = "*Source: IOM Displacement Tracking Matrix*",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 12, color = "grey30"),
      plot.caption  = element_markdown(size = 9, color = "grey50", hjust = 0)
    )
  )

# MISSING IN SOURCE? The displayed figure is figures/figure_6_combined_plot.png but
# this chunk does not save combined_plot. Add your ggsave() call here, e.g.:
# ggsave("projects/29-03-2026-South-Sudan-Violence/figures/figure_6_combined_plot.png",
#        combined_plot, width = 12, height = 10)
```

]
#block[
#block[
#set text(weight: "bold"); Figure 5 - IDPs in Budi and Ikotos counties by year
]
#box(image("figures/figure_6_combined_plot.png"))

]
= Alternative explanations
<alternative-explanations>
To provide alternative explanations for the pattern we see in the two previous plots --- increased levels of violence during project implementation --- I will look at drought levels as an indicator for a tentative effect of climate change, thereby allowing us to understand the correlation between the two variables and the potential impact on the project. As a data source I will use the SPEI index, which is derived using precipitation and potential evapotranspiration to determine drought at a spatial resolution of 0.5 degrees (3,071.756 $k m^2$ in South Sudan). The index ranges from -2 to 2, where 2 corresponds to an extremely wet situation and -2 to an extremely dry one, calculated with respect to the normal condition. From figure 6 it becomes clear that Budi and Ikotos have experienced sustained and more severe drought, as indicated by both the decrease in median SPEI and the decrease in variation in the measure. The grid-cell area quoted above is computed in the code chunk below.

#block[
```r
# Approximate area (km^2) of a 0.5-degree SPEI grid cell at ~4.26 deg latitude
(0.5 * 111) * (0.5 * 111 * cos(4.25611 * pi / 180))
```

]
#block[
```r
# MISSING IN SOURCE: the code that loads the SPEI raster, clips it to Budi/Ikotos
# and produces figures/figure_7_spei_boxplot_horizontal.png is not included in this
# post. Paste your SPEI extraction + ggplot boxplot code here so the figure is
# reproducible.
```

]
#block[
#block[
#set text(weight: "bold"); Figure 6 - SPEI drought index in Budi and Ikotos counties by year
]
#box(image("figures/figure_7_spei_boxplot_horizontal.png"))

]
= Spatial location of violent events and peacebuilding projects
<spatial-location-of-violent-events-and-peacebuilding-projects>
Another important measure to apply when investigating the effect of peacebuilding interventions is the spatial location of these interventions coupled with the location of armed conflicts. This is an approach I applied as part of the independent evaluation of the SDC's engagement in the area of peacebuilding, in its focus on Mozambique, where I, based on project reports and open sources, gathered information on the location of different supported peacebuilding interventions at the lowest administrative level in Mozambique. The code below reproduces figure 7, which displays the number of violent events in Mozambique as recorded by ACLED using a hexagonal grid, together with the locations of supported peacebuilding projects at the lowest administrative level. Analysing this data across years allows us to investigate how patterns of violence have changed both temporally and spatially.

#block[
```r
### Prerequisites
library(tidyverse)
library(sf)
library(ggrepel)
library(rgeoboundaries)
library(RColorBrewer)
library(classInt)     # for classIntervals() (Jenks breaks)
library(ggnewscale)   # for new_scale_colour() (two colour scales on one map)

# Loading administrative base maps
mozambique_ADM1_sf <- geoboundaries("Mozambique", "adm1")
mozambique_ADM2_sf <- geoboundaries("Mozambique", "adm2")
mozambique_ADM0_sf <- geoboundaries("Mozambique", "adm0")

# Loading capital and administrative cities in Mozambique
moz_cities <- read_csv("data/map/mozambique_cities.csv") |> 
  filter(capital %in% c("primary", "admin")) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(mozambique_ADM2_sf))

## Reading ACLED data
path_acled <- "your path to ACLED"

ACLED_df <- read_csv(path_acled) |> 
  filter(country == "Mozambique") |> 
  filter(!(sub_event_type %in% c("Government regains territory", 
                                 "Non-state actor overtakes territory", 
                                 "Government overtakes territory", 
                                 "Non-state actor overtakes territory", 
                                 "Sexual violence", 
                                 "Government regains territory", 
                                 "Abduction/forced disappearance")))

# Converting to a spatial object
ACLED_sf <- st_as_sf(ACLED_df, 
  coords = c("longitude", "latitude"), 
  crs = st_crs(mozambique_ADM0_sf))

# Create union of all Mozambique boundaries
mozambique_boundary <- st_union(mozambique_ADM0_sf)

# Create hexagonal grid over Mozambique
hex_grid <- st_make_grid(
  mozambique_boundary,
  cellsize = 0.3,
  square = FALSE,
  what = "polygons"
) |>
  st_sf() |>
  mutate(hex_id = row_number())

# Clip hexagons to Mozambique boundary
hex_grid_clipped <- st_intersection(hex_grid, mozambique_boundary)

#######
### Categorize conflicts 
#######

## Creating year intervals and counts of conflicts per hexagon

acled_per_hex_v2 <- hex_grid |>
  st_join(ACLED_sf) |>
  st_drop_geometry() |>
  filter(!is.na(year)) |>
  # Create year groups
  mutate(
    year_group = case_when(
      year <= 2017 ~ "2014-2017",
      year <= 2021 ~ "2018-2021",
      year <= 2024 ~ "2022-2024",
    ),
    year_group = factor(year_group, 
                        levels = c("2014-2017", "2018-2021", "2022-2024"))
  ) |>
  # Count by hex and year_group (aggregates across years within group)
  count(hex_id, year_group, name = "n_conflicts") |>
  complete(
    hex_id = unique(hex_grid$hex_id),
    year_group = factor(c("2014-2017", "2018-2021", "2022-2024"),
                        levels = c("2014-2017", "2018-2021", "2022-2024")),
    fill = list(n_conflicts = 0)
  )

# Join counts back to hex grid
hex_counts_year <- hex_grid_clipped |>
  left_join(acled_per_hex_v2, by = "hex_id")

## Categorize conflicts
# Compute Jenks (natural-breaks) class intervals on the conflict counts and print
# them. The printed break points are what the hard-coded category bounds below
# (1 / 4 / 18 / 39) are rounded from — inspect `breaks` and adjust the case_when()
# thresholds if you re-run on different data.
breaks <- classIntervals(hex_counts_year$n_conflicts, n = 5, style = "jenks")
print(breaks)

# Apply breaks
hex_counts_year <- hex_counts_year |>
  mutate(
    # Conflict categories read off the Jenks breaks above
    conflict_category = case_when(
      n_conflicts < 2 ~ "Very Low (0-1)",
      n_conflicts <= 4 ~ "Low (2-4)",
      n_conflicts <= 18 ~ "Medium (5-18)",
      n_conflicts <= 39 ~ "High (19-39)",
      TRUE ~ "Very High (39+)"
    ),
    conflict_category = factor(conflict_category, 
                               levels = c("Very Low (0-1)", "Low (2-4)", 
                                         "Medium (5-18)", "High (19-39)", 
                                         "Very High (39+)"))
  )

# Combining data (sum conflict counts per hexagon across all year groups)
hex_counts_total <- hex_counts_year |> 
  group_by(hex_id) |> 
  summarise(n_conflicts = sum(n_conflicts), .groups = "drop")

# Apply breaks
hex_counts_total <- hex_counts_total |>
  mutate(
    # Conflict categories read off the Jenks breaks above
    conflict_category = case_when(
      n_conflicts < 2 ~ "Very Low (0-1)",
      n_conflicts <= 4 ~ "Low (2-4)",
      n_conflicts <= 18 ~ "Medium (5-18)",
      n_conflicts <= 39 ~ "High (19-39)",
      TRUE ~ "Very High (39+)"
    ),
    conflict_category = factor(conflict_category, 
                               levels = c("Very Low (0-1)", "Low (2-4)", 
                                         "Medium (5-18)", "High (19-39)", 
                                         "Very High (39+)"))
  )

#########
###### Creating project locations data
#########

# MISSING IN SOURCE: replace this placeholder with the actual project-locations
# data frame. It must contain at least: name (ADM2 municipality), project_name,
# and peace ("Yes"/"No"). The map code below relies on these columns.
project_final_df_new <- "Data on locations of projects"

# Calculate centroids for municipalities
centroids_adm2 <- st_centroid(mozambique_ADM2_sf)
coords_adm2 <- st_coordinates(centroids_adm2)

# Get unique municipalities from your data
project_location_muni <- project_final_df_new |> 
  distinct()

# Create municipalities dataframe with coordinates
municipalities <- data.frame(
  municipality = unique(project_location_muni$name),
  lat = coords_adm2[match(unique(project_location_muni$name), 
                         mozambique_ADM2_sf$shapeName), 2],  # Adjust column name if needed
  lon = coords_adm2[match(unique(project_location_muni$name), 
                         mozambique_ADM2_sf$shapeName), 1]
)

# Join coordinates to project data
project_location_muni <- project_location_muni |> 
  left_join(municipalities, by = c("name" = "municipality"))


# Calculate offsets for multiple projects in same municipality
project_location_muni <- project_location_muni |> 
  group_by(name) |> 
  mutate(
    n_projects = n(),
    projects_index = row_number(),
    x_offset = (projects_index - (n_projects + 1)/2) * 0.15  # Smaller offset for denser map
  ) |> 
  ungroup()


# Separate the data
peace_data <- project_location_muni[project_location_muni$peace == "Yes",]
non_peace_data <- project_location_muni[project_location_muni$peace == "No",]

#########
###### Create reduced non-peace data (one per municipality)
#########
non_peace_data_reduced <- non_peace_data |>
  group_by(name) |>
  slice(1) |>
  ungroup()

#########
###### Set up separate palettes
#########
peace_projects <- unique(peace_data$project_name)
peace_palette <- brewer.pal(n = length(peace_projects), name = "Dark2")
names(peace_palette) <- peace_projects

non_projects <- unique(non_peace_data$project_name)  # Use full data for palette
non_peace_palette <- rep("gray50", length(non_projects))
names(non_peace_palette) <- non_projects

#########
###### Create the map
#########
map_adm2_vio <- ggplot() +

  # Hexagonal heatmap (clipped to Mozambique)
  geom_sf(data = hex_counts_total, 
          aes(fill = conflict_category),  # use conflict_category instead of n_conflicts ***
          color = NA, 
          alpha = 0.8) +
  scale_fill_brewer(  # ***  use scale_fill_brewer instead of scale_fill_distiller ***
    name = "Conflict Intensity\n(number of conflicts)",
    palette = "YlOrRd",  # *** Yellow-Orange-Red palette ***
    direction = 1
  ) +
  
  # ADM boundaries
  geom_sf(data = mozambique_ADM2_sf, fill = NA, color = "gray70", linewidth = 0.2) +
  geom_sf(data = mozambique_ADM1_sf, fill = NA, color = "gray50", linewidth = 0.8) +  
  
  # Non-peace projects (circles, gray) 
  geom_point(data = non_peace_data_reduced,
             aes(x = lon + x_offset, 
                 y = lat, 
                 colour = project_name),
             shape = 16,
             size = 4,  
             alpha = 0.5) + 
  # Use limits to include ALL non-peace projects in legend
  scale_colour_manual(values = non_peace_palette, 
                      name = "Other SDC Projects",
                      limits = non_projects) +
  guides(colour = guide_legend(order = 2, override.aes = list(shape = 16, size = 4))) +
  # Reset colour scale for second layer
  new_scale_colour() +

    # Peace-related projects (triangles, colored)
  geom_point(data = peace_data,
             aes(x = lon + x_offset, 
                 y = lat, 
                 colour = project_name),
             shape = 17,
             size = 4,  
             alpha = 0.9) + 
  scale_colour_manual(values = peace_palette, name = "Sampled Peacebuilding Projects") +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 17, size = 4))) +  

  # Major Cities
  geom_text_repel(data = moz_cities,
                  aes(label = city, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 3,
                  fontface = "bold",
                  color = "black",
                  bg.color = "white",
                  bg.r = 0.15,
                  nudge_x = 1,  
                  box.padding = 0.3,
                  point.padding = 0.2,
                  segment.size = 1,
                  segment.color = "gray30",
                  min.segment.length = 0.1,
                  max.overlaps = Inf) +
  
  # Styling
  theme_void() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 5)),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.background = element_blank(),
    legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  coord_sf()
```

]
#block[
#block[
#set text(weight: "bold"); Figure 7 - SDC peacebuilding projects and ACLED conflict intensity in Mozambique
]
#box(image("figures/figure_8_mozambique_ALL_projects_adm2.png"))

]
= Using satellites to measure conflict intensity
<using-satellites-to-measure-conflict-intensity>
A known caveat of the above sources of evidence is that they rely on humans having either experienced or gained knowledge about a violent event or IDP movements. This, like any other data source, creates measurement errors, since we might not capture violent conflicts in areas where first-hand accounts are difficult to obtain due to, for instance, lack of access \[CITATION NEEDED --- replace this placeholder\]. A way to circumvent this problem is to use the secondary effects of violent acts, namely the fires created as a result of such acts. This is done by using fires recorded by satellites and a machine learning model trained on past fires and daily weather patterns to classify whether a fire is caused by natural causes or by violent acts. This is an approach I have applied in Somalia when studying changes in territorial control (see #link("https://erik-h-k.github.io/projects/23-10-2023-territorial-control-somalia/index.html")[link];). A caveat of this approach is that acts of violence that do not involve things being ignited are not recorded by this method. Thus, we will systematically fail to record low-level forms of violence.

= Other sources
<other-sources>
- Surveys: Can be a usefull tool to investigate both current levels of violence, but also whether former combants' view of violence has changed. Enabling one to assess whether the peacebuilding intervention's focus on rehabilitation was succesful.
- Hate speach detection:
- The number of Peace settlements:
- Duration since last violent act:
- Destruction of buildings:
-

 
  
#set bibliography(style: "american-political-science-review.csl") 


#bibliography("my\_zotero\_library.bib")

