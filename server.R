# FOIA MINER
# server.R
# Chicago Department of Innovation and Technology
#
# Dashboard to track 1) most popular words in FOIA requests
# from selected time period, 2) how the most popular words change
# each month, 3) the volume of FOIA requests by month, and 4) words that
# demonstrate a statistially significant rise in volume when compared to
# their average frequency in previous six months.
# Dashboard built for several City of Chicago Departments.

################################################################################

#install and load libraries
source("startup.R")
source("dash_URLs.R")
library(tm)
library(RSocrata)
library(utils)
library(qlcMatrix)
library(ggplot2)
library(shiny)
library(data.table)
library(lubridate)
library(devtools)

#source code
shinyServer(function(input, output) {

  cleanlist <- reactive({

    #read entire dataset with URL
    full_FOIA = read.socrata(SODA_URL[input$department])

    #subtract six months from selected date range (for rolling mean and stdev)
    first_date <- as.POSIXct(input$date[1])
    first_date <- as.POSIXct(first_date)
    first_date <- first_date - months(6)

    #subset data frame for date range user selects
    rec_FOIA <- full_FOIA[full_FOIA$DATE.RECEIVED >= as.POSIXct(input$date[1]) & full_FOIA$DATE.RECEIVED <= as.POSIXct(input$date[2]),]

    #subset data frame for date range user select and six previous months (for rising word analysis)
    FOIA <- full_FOIA[full_FOIA$DATE.RECEIVED >= first_date & full_FOIA$DATE.RECEIVED <= as.POSIXct(input$date[2]),]

    #store all dates for requests made in selected date range
    dates = as.vector(rec_FOIA$DATE.RECEIVED)

    ##====================================================================
    # REQUEST VOLUME TAB
    # most recent request, numer of requests in date range, and
    # data frame of request volume by month

    #find most recent FOIA request to display
    max = max(full_FOIA$DATE.RECEIVED)

    #find number of requests in selected date range
    request_count = length(rec_FOIA$DATE.RECEIVED)

    #strip date column of all days
    dates = strftime(dates, format="%Y/%m")

    #create data frame for request volume by month
    date_freq = data.frame(table(dates))

    #rename columns
    colnames(date_freq) <- c("Month", "Requests")

    ##====================================================================
    # TOP WORDS OVERALL TAB
    # data frame of top 5 keywords in requests and their frequencies

    #create one corpus of all requests
    requests = paste(rec_FOIA$DESCRIPTION.OF.REQUEST, collapse=" ")

    #create source
    vec_source = VectorSource(requests)

    #create corpus
    corpus = Corpus(vec_source)

    #turn all words into lower case
    corpus = tm_map(corpus, content_transformer(tolower))

    #get rid of all white space
    corpus = tm_map(corpus, stripWhitespace)

    #turn selected characters into spaces
    for(j in seq(corpus))
    {
      corpus[[j]] <- gsub("/", " ", corpus[[j]])
      corpus[[j]] <- gsub(",", " ", corpus[[j]])
      corpus[[j]] <- gsub("'s", " ", corpus[[j]])
      corpus[[j]] <- gsub("&", " ", corpus[[j]])
      corpus[[j]] <- gsub(";", " ", corpus[[j]])
    }

    #turn all synonyms into one common term
    for(j in seq(corpus))
    {
      corpus[[j]] <- gsub("rlc video footage", "rlc_video_footage", corpus[[j]])
      corpus[[j]] <- gsub("video footage", "rlc_video_footage", corpus[[j]])
      corpus[[j]] <- gsub("violations", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("vehicle tow reports", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("vehicle tow records", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("vehicle tow, impound, release reports", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("vehicle tow, impound,release reports", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("vehicle tow,impound, release reports", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("vehicle tow,impound,release reports", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("tow and impound reports", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("vehicle, tow impound release records", "tow_impound_release", corpus[[j]])
      corpus[[j]] <- gsub("arrest report", "arrest_report", corpus[[j]])
      corpus[[j]] <- gsub("environmental records", "environmental_records", corpus[[j]])
      corpus[[j]] <- gsub("food inspections", "food_inspection", corpus[[j]])
      corpus[[j]] <- gsub("food inspection", "food_inspection", corpus[[j]])
      corpus[[j]] <- gsub("c of o", "c/o", corpus[[j]])
      corpus[[j]] <- gsub("c o", "c/o", corpus[[j]])
      corpus[[j]] <- gsub("viol", "vio", corpus[[j]])
      corpus[[j]] <- gsub("vioations", "vio", corpus[[j]])
      corpus[[j]] <- gsub("vioation", "vio", corpus[[j]])
      corpus[[j]] <- gsub("permits", "permit", corpus[[j]])
      corpus[[j]] <- gsub("licenses", "license", corpus[[j]])
      corpus[[j]] <- gsub("applications", "application", corpus[[j]])
      corpus[[j]] <- gsub("emails", "email", corpus[[j]])
      corpus[[j]] <- gsub("fie", "file", corpus[[j]])
      corpus[[j]] <- gsub("tickets", "ticket", corpus[[j]])
    }

    #stem words to combine terms like "violation/violations" and "train/training"
    corpus = tm_map(corpus, stemDocument)

    #function to remove words we don't want
    removed <- function(corp,
                        rmwords = c("2010", "2011", "2012", "2013","2014","2015","regarding",
                                    "information", "ave.","st.","city", "chicago","street",
                                    "intersection", "reports", "records", "report", "record",
                                    "documents","info","request","requesting","record", "january",
                                    "february","march","april","may","june","july","august","september",
                                    "october","november","december","jan","feb","mar","apr","jun","jul","aug",
                                    "sep","nov","oct","nov","dec","locate","business","please","ave","st","mayor",
                                    "office","provide","emanuel", "rahm", "311", "foia", "including","located", "sent","copies",
                                    "received","department", "following","include", "relating",
                                    "pertaining","referenced","related","foia.","freedom","act","pursuant","like",
                                    "approximate","around", "requests","used","since","use","choose","underlying",
                                    "within","public","library","nbsp","ldquo","illinois", "also","rdquo","rsquo","&quot;",
                                    "-1","-2"),
                        more = NULL){
      ret <- tm_map(x = corp,
                    FUN = removeWords,
                    words = c(rmwords, more))
      return(ret)
    }

    #call function to remove words
    corpus <- removed(corp = corpus)

    #remove common english words like "the" and "on"
    corpus = tm_map(corpus, removeWords, stopwords("english"))

    #ensure that DocumentTermMatrix receives a text document
    corpus = tm_map(corpus, PlainTextDocument)

    #create matrix of document terms
    dtm = DocumentTermMatrix(corpus)

    #convert document term matrix into normal matrix
    dtm2 = as.matrix(dtm)

    #column sums of this matrix
    freq_terms = colSums(dtm2)

    #sort vector to see most frequent terms
    freq_terms = sort(freq_terms, decreasing=TRUE)

    #create data frame of top 5 words and their frequencies
    freq = head(freq_terms, 5)
    word = names(freq)
    wf <- data.frame(word, freq=freq)

    ##====================================================================
    # TOP WORDS BY MONTH TAB
    # dataframe of top five word frequencies by month

    #create vector of top five keywords by name
    top_words = names(freq)

    #create vector of factors and dates (to make columns in dataframe)
    word_factor = c()
    word_dates = c()

    #vector of dates
    dates = as.character(rec_FOIA$DATE.RECEIVED)

    #convert all Descriptions to lower case
    descriptions = tolower(rec_FOIA$DESCRIPTION.OF.REQUEST)

    #convert all spaces to NA
    descriptions = gsub(" +", " ", descriptions)
    descriptions[descriptions == ""] <- NA
    descriptions[descriptions == " "] <- NA

    #compare keywords to descriptions
    ii = c()
    word_factor = c()
    for(k in 1:length(top_words)){
      ii <- sim.strings(top_words[k], descriptions) > 0.15
      ii[ii == TRUE] <- top_words[k]
      df = data.frame(ii, dates)
      df.sub = subset(df, ii != FALSE)
      word_dates = c(word_dates, as.vector(df.sub$dates))
      word_factor = c(word_factor, ii[-grep(FALSE, ii)])
    }

    #create data frame with just date and factor as columns
    factor_and_dates = data.frame(word_factor, word_dates)

    #strip date column of all years
    factor_and_dates$word_month = strftime(factor_and_dates$word_dates, format="%Y/%m")

    #create data frame for word frequencies by month
    word_freq = data.frame(table(factor_and_dates$word_factor, factor_and_dates$word_month))

    #rename columns
    colnames(word_freq) <- c("Word", "Month", "Freq")

  ##====================================================================
  # RISING WORDS TAB
  # Find months in which any of the top 20 words demonstrate a statistically
  # significant rise in volume and store each month, word, frequency triple
  # in data frame

    #create vector of 20 most frequent words in date range user selects
    freq.l = head(freq_terms, 20)
    word.l = names(freq.l)
    wf.l <- data.frame(word.l, freq.l=freq.l)

    #create vector of top 20 keywords by name
    top_words.l = names(freq.l)

    #create vector of factors and dates (to make columns in dataframe)
    word_factor.l = c()
    word_dates.l = c()

    #vector of dates that includes six months before date range user selects
    dates.l = as.character(FOIA$DATE.RECEIVED)

    #convert all descriptions to lower case
    descriptions.l = tolower(FOIA$DESCRIPTION.OF.REQUEST)

    #convert all spaces to NA
    descriptions.l = gsub(" +", " ", descriptions.l)
    descriptions.l[descriptions.l == ""] <- NA
    descriptions.l[descriptions.l == " "] <- NA

    #compare top 20 words to descriptions
    ii = c()
    word_factor.l = c()
    for(k in 1:length(top_words.l)){
      ii <- sim.strings(top_words.l[k], descriptions.l) > 0.15
      ii[ii == TRUE] <- top_words.l[k]
      df = data.frame(ii, dates.l)
      df.sub = subset(df, ii != FALSE)
      word_dates.l = c(word_dates.l, as.vector(df.sub$dates.l))
      word_factor.l = c(word_factor.l, ii[-grep(FALSE, ii)])
    }

    #create data frame with just word and date as columns
    factor_and_dates.l = data.frame(word_factor.l, word_dates.l)

    #strip date column of all years
    factor_and_dates.l$word_month.l = strftime(factor_and_dates.l$word_dates.l, format="%Y/%m")

    #create data frame for word frequencies by month
    word_freq.l = data.frame(table(factor_and_dates.l$word_factor.l, factor_and_dates.l$word_month.l))

    #rename columns
    colnames(word_freq.l) <- c("Word", "Month", "Freq")

    #store word frequencies by month in data table to find rolling mean and stdev
    dat <- word_freq.l
    dat <- as.data.table(dat)

    # make sure dat is complete
    full_examp <- as.data.table(expand.grid(
      Month = dat[ , .N, keyby = Month][ , as.character(Month)],
      Word = dat[ , .N, keyby = Word][ , as.character(Word)]))
    dat <- merge(dat, full_examp, by = c("Month", "Word"), all.y = T)
    rm(full_examp)

    #store variable for previous months
    NPER <- 6

    #calculate moving ave & stdev by word
    dat1 <- dat[i = TRUE,
                j = list(MovingAve = as.numeric(filter(x = Freq,
                                                       filter = rep(1 , NPER),
                                                       sides = 1)) / NPER,
                         X1 = as.numeric(filter(x = Freq ^ 2,
                                                filter = rep(1 , NPER),
                                                sides = 1)),
                         X2 = as.numeric(filter(x = Freq,
                                                filter = rep(1 , NPER),
                                                sides = 1) ^ 2) / NPER,
                         Freq,
                         Month,
                         Word),
                by = Word]
    mvstdev = dat1[ , list(mvstdev = sqrt((X1 - (X2)) / (NPER - 1)))]
    dat1$mvstdev <- mvstdev

    #new column to display 0/1 for statistically insignificant/significant rise
    dat1[, significant := +(((1.96* mvstdev) + MovingAve) < Freq)]

    #create data frame subset by statistical significance
    dat1.df = data.frame(dat1)
    sig.df <- subset(dat1.df, significant == 1)
    sig.df = sig.df[order(sig.df$Month),]

    #remove unnecessary columns from sig.df to display dataframe to user in table
    display.df <- sig.df
    keep <- c("Month","Word","Freq")
    display.df = display.df[keep]
    display.df = data.frame(display.df$Month, display.df$Word, display.df$Freq)
    colnames(display.df) <- c("Month", "Word", "Freq")

    ##====================================================================
    #RETURN LIST OF VARIABLES FROM CODE ABOVE TO RENDER PLOTS, TEXT, AND TABLES
    return(list(wf=wf, freq=freq, word_freq=word_freq, freq.l=freq.l, max=max, request_count=request_count,
                date_freq=date_freq, sig.df=sig.df, word=word, display.df=display.df))
  })

################################################################################
# RENDER PLOTS, TABLES, AND TEXT TO DASHBOARD

    #plot top five words from requests in selected date range
    output$overall_plot <- renderPlot({

      cleanlist <- cleanlist()
      plot = ggplot(subset(cleanlist$wf, cleanlist$freq>0),
                    aes(word, freq))
      plot = plot + geom_bar(stat="identity", fill = "blue")
      plot = plot + theme(axis.text.x=element_text(angle=45, hjust=1))
      plot = plot + labs(title = "Most Frequent Words\n", x = "\nWords", y = "Frequency\n")
      plot = plot + theme(plot.title = element_text(size = 22, face = "bold"))
      print(plot)
    })

    #plot changes in top five words by month
    output$month_plot <- renderPlot({
      cleanlist <- cleanlist()
      plot = ggplot(data <- cleanlist$word_freq) +
        aes(x = Month,
            y = Freq,
            fill = Word,
            label = Freq) +
        geom_bar(stat = "identity", position = "dodge")
      plot = plot + labs(title = "Most Frequent Words by Month\n", x = "\nMonth", y = "Frequency\n")
      plot = plot + theme(plot.title = element_text(size = 22, face = "bold"))
      print(plot)
    })

    #allow user to download CSV of top twenty words
    output$overall_data <- downloadHandler(

      filename = function() {
        paste(input$department, "_TopWords",".csv", sep = "")
      },
      content = function(con) {
        cleanlist <- cleanlist()
        overall.df = data.frame(cleanlist$freq.l)
        colnames(overall.df) <- c("Word")
        write.csv(overall.df, con)
      }
    )

    #allow user to download CSV of changes by month in top five words
    output$month_data <- downloadHandler(
      filename = function() {
        paste(input$department, "_TopWordsByMonth",".csv", sep = "")
      },
      content = function(con) {
        cleanlist <- cleanlist()
        write.csv(cleanlist$word_freq, con)
      }
    )

    #print total FOIA requests received by dep't in selected date range
    output$rec_req <- renderText({
      cleanlist <- cleanlist()
      paste0("Total FOIA Requests in Date Range: ", cleanlist$request_count,".")
    })

    #print most recent FOIA request received by dep't
    output$volume_text <- renderText({
      cleanlist <- cleanlist()
      paste0("Most recent FOIA request: ", cleanlist$max,".")
    })

    #plot volume of FOIA requests by month by dep't
    output$volume_plot <- renderPlot({
      cleanlist <- cleanlist()
      plot = ggplot(data = cleanlist$date_freq, aes(x=Month, y=Requests, group=1)) +
        geom_line(size = 2, colour = "blue") +
        geom_point()
      plot = plot + labs(title = "Total FOIA Requests\n", x = "\nMonth", y = "Number of Requests\n")
      plot = plot + theme(plot.title = element_text(size = 22, face = "bold"))
    print(plot)
    })

    #allow user to download volume of requests by dep't by month
    output$volume_data <- downloadHandler(
      filename = function() {
        paste(input$department, "_Volume",".csv", sep = "")
      },
      content = function(con) {
        cleanlist <- cleanlist()
        write.csv(cleanlist$date_freq, con)
      }
    )

    #print explanation of rising words plot
    output$rising_explanation <- renderText({
      "The plot and table below display words (from the top 20) that exhibit a
      statistically significant rise in frequency when compared to their average
      frequency over the previous six months."
    })

    #plot words that rise in popularity when compared to previous six months
    output$rising_plot <- renderPlot({
      cleanlist <- cleanlist()
      plot = ggplot(data = cleanlist$sig.df, aes(x = Month, y = Freq, fill = Word, label = Freq)) +
        geom_bar(stat = "identity", position = "dodge")
      plot = plot + labs(title = "Rising Keywords\n", x = "\nMonth", y = "Frequency\n")
      plot = plot + theme(plot.title = element_text(size = 22, face = "bold"))
      print(plot)
    })

    #plot table of rising words by displaying month/word pairs
    output$rising_table <- renderTable({
      cleanlist <- cleanlist()
      cleanlist$display.df
    })

    #allow users to download CSV of month/word pairs for rising words
    output$rising_data <- downloadHandler(
      filename = function() {
        paste(input$department, "_RisingWords",".csv", sep = "")
      },
      content = function(con) {
        cleanlist <- cleanlist()
        write.csv(cleanlist$display.df, con)
      }
    )
})
