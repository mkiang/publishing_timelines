## Imports ----
library(tidyverse)
library(readxl)
library(here)
source(here("code", "mk_nytimes.R"))

## Read raw data ----
pubs <- read_excel(here("data", "tracking_pubs_hashed.xlsx"))

## Make labels ----
pub_labs <- pubs %>%
    group_by(paper) %>%
    summarize(
        time = max(date_delta),
        rejects = sum(grepl("Reject", event)),
        desk_rejects = sum(grepl("Reject without", event))
    ) %>%
    ungroup() %>%
    mutate(n_id = sample(1:n_distinct(pubs$paper))) %>%
    mutate(
        paper_lab = sprintf(
            "Paper %i\n%i days / %i rejection%s (%i desk)",
            n_id,
            time,
            rejects,
            ifelse(rejects > 1, "s", ""),
            desk_rejects
        )
    ) %>%
    arrange(n_id)

## Clean up ----
pubs <- pubs %>%
    left_join(pub_labs) %>%
    mutate(paper_cat = factor(paper_lab,
                              levels = pub_labs$paper_lab,
                              ordered = TRUE)) %>%
    ## Make a factor version (need ordering for color palette)
    mutate(event_cat = factor(
        event,
        levels = c(
            "Initial commit",
            "First draft",
            "Submitted",
            "Submitted after appeal",
            "Reject without review",
            "Reject after review",
            "Revise and resubmit",
            "Resubmitted",
            "Accepted",
            "Published"
        ),
        ordered = TRUE
    )) %>%
    ## Y-axis for keeping things mostly parallel
    mutate(y_axis = case_when(
        event %in% c("Submitted", "Submitted after appeal", "Resubmitted") ~ -1,
        event %in% c(
            "Reject without review",
            "Reject after review",
            "Revise and resubmit",
            "Initial commit",
            "First draft"
        ) ~ 0,
        event %in% c("Accepted", "Published") ~ 1
    )) %>%
    ## Y-axis for showing rejections more clearly
    mutate(y_axis_alt = case_when(
        event %in% c("Initial commit",
                     "First draft") ~ 0,
        event %in% c(
            "Submitted",
            "Submitted after appeal",
            "Reject without review",
            "Reject after review",
            "Revise and resubmit",
            "Resubmitted"
        ) ~ -1 * counter,
        event %in% c("Accepted", "Published") ~ 1
    ))

## Make a color palette and shapes ----
event_col <-
    c(
        "black",
        "#313695",
        "#FDAE61",
        "#FDAE61",
        "#A50026",
        "#D73027",
        "#7FBC41",
        "#7FBC41",
        "#4D9221",
        "#276419"
    )

event_shapes <-
    c(5,
      18,
      19,
      1,
      4,
      8,
      10,
      19,
      17,
      15)

## Plot with everything parallel ----
## People found this one too confusing
p1 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis,
        color = event_cat,
        group = paper_cat,
        shape = event_cat
    )
) +
    geom_step() +
    geom_point(size = 3.5, alpha = .75) +
    facet_grid( ~ paper_cat, scales = "free") +
    mk_nytimes(legend.position = "right",
               legend.justification = .5) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-1, 0, 1),
        labels = c("With Journal", "With Me", "Out"),
        expand = c(0, .1)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual("Event", values = event_col) +
    scale_shape_manual("Event", values = event_shapes) 

## Drop down with each rejection ----
p1_alt <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat
    )
) +
    geom_step() +
    geom_point(size = 3.5, alpha = .75) +
    facet_grid( ~ paper_cat, scales = "free") +
    mk_nytimes(legend.position = "right",
               legend.justification = .5) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .25)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual("Event", values = event_col) +
    scale_shape_manual("Event", values = event_shapes) +
    labs(title = "Paper trajectories: From analysis to publication",
         subtitle = "How long does it take to publish a paper? How many tries will it take?",
         caption = "A small (but probably representative) sample of my papers published between 2019 and 2021.")

## Save ----
ggsave(
    here("plots", "paper_traj_20211112.jpg"), 
    p1_alt,
    width = 9,
    height = 3.5,
    scale = 1.25,
    dpi = 300
)
