

assioma <- read.delim("fit_assioma.txt", header = FALSE)

magene <- read.delim("fit_magene.txt", header = FALSE)


sep_assioma <- which(assioma[[1]] == "---")

sep_magene <- which(magene[[1]] == "---")


# convert to a better df

assioma_list <- strsplit(assioma[,1], ": ")


assioma_df <- data.frame(
  par = unlist(lapply(assioma_list, function(x) x[1])),
  value = unlist(lapply(assioma_list, function(x) x[2]))
)

magene_list <- strsplit(magene[,1], ": ")


magene_df <- data.frame(
  par = unlist(lapply(magene_list, function(x) x[1])),
  value = unlist(lapply(magene_list, function(x) x[2]))
)




# get heart rate, power and , time

assioma_extract <- list()

pb = pbapply::timerProgressBar(min = 1, max = length(sep_assioma), initial = 1, width = 20)
for (i in 1:length(sep_assioma)) { # where "---" begins
  j <- sep_assioma[i] + 1
  df <- data.frame(power = NA, hr = NA, time = NA)
  while (j != sep_assioma[i+1]) { # where next "---" starts
    if(assioma_df$par[j] %in% " * power")  {df$power <-  assioma_df$value[j]}
    if(grepl("heart_rate", assioma_df$par[j]))  {df$hr <-  assioma_df$value[j]}
    if(grepl("timestamp", assioma_df$par[j]))  {df$time <-  assioma_df$value[j]}
    assioma_extract[[i]] <- df
    j <- j + 1
  }


  pbapply::setTimerProgressBar(pb, i)
}

assioma_par <- do.call( rbind.data.frame, assioma_extract)



magene_extract <- list()

pb = pbapply::timerProgressBar(min = 1, max = length(sep_magene), initial = 1, width = 20)
for (i in 1:length(sep_magene)) { # where "---" begins
  j <- sep_magene[i] + 1
  df <- data.frame(power = NA, hr = NA, time = NA)
  while (j != sep_magene[i+1]) { # where next "---" starts
    if(magene_df$par[j] %in% " * power")  {df$power <-  magene_df$value[j]}
    if(grepl("heart_rate", magene_df$par[j]))  {df$hr <-  magene_df$value[j]}
    if(grepl("timestamp", magene_df$par[j]))  {df$time <-  magene_df$value[j]}
    magene_extract[[i]] <- df
    j <- j + 1
  }
  
  
  pbapply::setTimerProgressBar(pb, i)
}

magene_par <- do.call( rbind.data.frame, magene_extract)



# matrix alignment


magene_par <- magene_par[which(magene_par$time %in% intersect(magene_par$time, assioma_par$time)),]

assioma_par <- assioma_par[which(assioma_par$time %in% intersect(magene_par$time, assioma_par$time)),]


# add lap info
magene_par$lap <- seq(1:nrow(magene_par)) 
assioma_par$lap <- seq(1:nrow(assioma_par))

fin <- 
min(nrow(assioma_par), nrow(magene_par))

df <- 
data.frame(time = as.character(assioma_par$lap[1:fin]),
           power_assioma = assioma_par$power[1:fin], 
           power_magene = magene_par$power[1:fin]#,
           #hr_assioma = assioma_par$hr[1:fin],      # no need to analysis, same
           #hr_magene = magene_par$hr[1:fin]      # no need to analysis, same
           ) 

df$power_assioma <- as.numeric(unlist(lapply(strsplit(df$power_assioma, " \\("), function(x) x[[1]])))
df$power_magene <- as.numeric(unlist(lapply(strsplit(df$power_magene, " \\("), function(x) x[[1]])))
#df$hr_assioma <- as.numeric(unlist(lapply(strsplit(df$hr_assioma, " \\("), function(x) x[[1]])))
#df$hr_magene <- as.numeric(unlist(lapply(strsplit(df$hr_magene, " \\("), function(x) x[[1]])))

p <- plotly::ggplotly(df %>% reshape2::melt() %>% ggplot(aes(x = time, y = value, color = variable)) +
   geom_point(alpha = .7) + 
  geom_line(aes(group=1)), alpha = .7 )


htmlwidgets::saveWidget(p, "comparison.html", selfcontained=FALSE)
            