library(rtweet)

tweets <- get_timeline(c("Microsoft", "generalelectric", "PTC", "Siemens", "siemensindustry",
                         "Uptake", "nvidia", "ROKAutomation",
                         "Accenture", "AccentureAI", "AccentureTech", "AccentureDigi", "Cognex_Corp",
                         "SAP", "SAPNews", "SAPhightech", "SAPCommunity", "Google", "Tesla", "intel",
                         "PwC", "KPMG", "IBM", "IBMResearch", "IBMAnalytics"), n = 3000)

table(tweets$screen_name)
