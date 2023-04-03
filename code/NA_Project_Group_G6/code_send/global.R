# ------------------------------------------------------------------------------
# 
# Title:        Exploring European Football Club Transfers since 1992
#
# Authors:      Florian Preiss (54385), Leon Kahrig (55584), 
#               Lilian Felmayer (54750), Maximilian Jagiello (53525), 
#               Hanna Baeuchle (53279)
#
# Date:         17-03-2023
#
# Course:       Network Analytics
# Supervisor:   Prof. Rodrigo Belo
# Assignment:   Group Project Shiny App
# Group:        G6
#
# File:         Global
# 
#               This file loads the prepared dataset and defines any global 
#               variables or functions needed by the app.
#
# ------------------------------------------------------------------------------


# Load necessary libraries -----------------------------------------------------
library(dplyr)
library(ggmap)
library(ggplot2)
library(igraph)
library(plotly)
library(RgoogleMaps)
library(shiny)
library(shinythemes)
library(visNetwork)


# Set bounding box for maps ----------------------------------------------------
bbox <- c(-180, -60, 180, 80)


# Retrieve the map using the Stamen map service --------------------------------
map <- get_stamenmap(bbox, zoom = 2, maptype = "toner-lite")


# Load data from CSV file ------------------------------------------------------
df.transfers.network <- read.csv(file = '../data_send/football-transfers.csv')


# Define variable names and descriptions ---------------------------------------
variable.names <- c("season", "window", "country", "country_lat", "country_lon",
                    "league", "club", "movement", "dealing_club",
                    "dealing_country", "dealing_country_lat",
                    "dealing_country_lon", "name", "age",
                    "nationality", "position", "market_value", "fee")

variable.desc <- c("The year of the season in which the trade took place.",
                   "The season of the year in which the trade took place.",
                   "The country to which the player was transferred.",
                   "The receiving country's latitude.",
                   "The receiving country's longitude.",
                   "The league to which the player was transferred.",
                   "The club to which the player was transferred.",
                   "The direction of the transfer record entry (in/out).",
                   "The club from which the player was transferred.",
                   "The country from which the player was transferred.",
                   "The dealing country's latitude.",
                   "The dealing country's longitude",
                   "The name of the player that was transferred.",
                   "The age of the player that was transferred.",
                   "The nationality of the player that was transferred.",
                   "The position of the player that was transferred.",
                   "The market value of the player that was transferred",
                   "The transfer fee of the player that was transferred")

# Create data frame with variable names and descriptions -----------------------
df.available.vars <- data.frame(Variable = variable.names,
                                Description = variable.desc)
