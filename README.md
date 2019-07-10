<!-- README.md is generated from README.Rmd. Please edit that file -->



fuzzyjoin: Join data frames on inexact matching
------------------

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fuzzyjoin)](https://cran.r-project.org/package=fuzzyjoin)
[![Travis-CI Build Status](https://travis-ci.org/dgrtwo/fuzzyjoin.svg?branch=master)](https://travis-ci.org/dgrtwo/fuzzyjoin)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dgrtwo/fuzzyjoin?branch=master&svg=true)](https://ci.appveyor.com/project/dgrtwo/fuzzyjoin)
[![Coverage Status](https://img.shields.io/codecov/c/github/dgrtwo/fuzzyjoin/master.svg)](https://codecov.io/github/dgrtwo/fuzzyjoin?branch=master)


The fuzzyjoin package is a variation on dplyr's [join](http://www.inside-r.org/node/230646) operations that allows matching not just on values that match between columns, but on inexact matching. This allows matching on:

* Numeric values that are within some tolerance (`difference_inner_join`)
* Strings that are similiar in Levenshtein/cosine/Jaccard distance, or [other metrics](http://finzi.psych.upenn.edu/library/stringdist/html/stringdist-metrics.html) from the [stringdist](https://cran.r-project.org/package=stringdist) package (`stringdist_inner_join`)
* A regular expression in one column matching to another (`regex_inner_join`)
* Euclidean or Manhattan distance across multiple columns (`distance_inner_join`)
* Geographic distance based on longitude and latitude (`geo_inner_join`)
* Intervals of (start, end) that overlap (`interval_inner_join`)
* Genomic intervals, which include both a chromosome ID and (start, end) pairs, that overlap (`genome_inner_join`)

One relevant use case is for classifying freeform text data (such as survey responses) against a finite set of options.

The package also includes:

* For each of `regex_`, `stringdist_`, `difference_`, `distance_`, `geo_`, and `interval_`, variations for the six dplyr "join" operations- for example,
  * `regex_inner_join` (include only rows with matches in each)
  * `regex_left_join` (include all rows of left table)
  * `regex_right_join` (include all rows of right table)
  * `regex_full_join` (include all rows in each table)
  * `regex_semi_join` (filter left table for rows with matches)
  * `regex_anti_join` (filter left table for rows without matches)
* A general wrapper (`fuzzy_join`) that allows you to define your own custom fuzzy matching function.
* The option to include the calculated distance as a column in your output, using the `distance_col` argument

### Installation

Install from CRAN with:


```r
install.packages("fuzzyjoin")
```

You can also install the development version from GitHub using [devtools](https://cran.r-project.org/package=devtools):


```r
devtools::install_github("dgrtwo/fuzzyjoin")
```

### Example of `stringdist_inner_join`: Correcting misspellings against a dictionary

Often you find yourself with a set of words that you want to combine with a "dictionary"- it could be a literal dictionary (as in this case) or a domain-specific category system. But you want to allow for small differences in spelling or punctuation.

The fuzzyjoin package comes with a set of common misspellings ([from Wikipedia](https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines)):


```r
library(dplyr)
library(fuzzyjoin)
data(misspellings)

misspellings
#> # A tibble: 4,505 x 2
#>    misspelling correct   
#>    <chr>       <chr>     
#>  1 abandonned  abandoned 
#>  2 aberation   aberration
#>  3 abilties    abilities 
#>  4 abilty      ability   
#>  5 abondon     abandon   
#>  6 about      about     
#>  7 abotu       about     
#>  8 abouta      about a   
#>  9 aboutit     about it  
#> 10 aboutthe    about the 
#> # ... with 4,495 more rows
```


```r
# use the dictionary of words from the qdapDictionaries package,
# which is based on the Nettalk corpus.
library(qdapDictionaries)
words <- tbl_df(DICTIONARY)

words
#> # A tibble: 20,137 x 2
#>    word  syllables
#>  * <chr>     <dbl>
#>  1 hm         1.00
#>  2 hmm        1.00
#>  3 hmmm       1.00
#>  4 hmph       1.00
#>  5 mmhmm      2.00
#>  6 mmhm       2.00
#>  7 mm         1.00
#>  8 mmm        1.00
#>  9 mmmm       1.00
#> 10 pff        1.00
#> # ... with 20,127 more rows
```

As an example, we'll pick 1000 of these words (you could try it on all of them though), and use `stringdist_inner_join` to join them against our dictionary.


```r
set.seed(2016)
sub_misspellings <- misspellings %>%
  sample_n(1000)
```


```r
joined <- sub_misspellings %>%
  stringdist_inner_join(words, by = c(misspelling = "word"), max_dist = 1)
```

By default, `stringdist_inner_join` uses optimal string alignment (Damerau–Levenshtein distance), and we're setting a maximum distance of 1 for a join. Notice that they've been joined in cases where `misspelling` is close to (but not equal to) `word`:


```r
joined
#> # A tibble: 728 x 4
#>    misspelling correct word    syllables
#>    <chr>       <chr>   <chr>       <dbl>
#>  1 sould       should  could        1.00
#>  2 sould       should  should       1.00
#>  3 sould       should  sold         1.00
#>  4 sould       should  soul         1.00
#>  5 sould       should  sound        1.00
#>  6 sould       should  would        1.00
#>  7 fiels       feels   field        1.00
#>  8 fiels       feels   fils         1.00
#>  9 conscent    consent consent      2.00
#> 10 fleed       freed   bleed        1.00
#> # ... with 718 more rows
```

#### Classification accuracy

Note that there are some redundancies; words that could be multiple items in the dictionary. These end up with one row per "guess" in the output. How many words did we classify?


```r
joined %>%
  count(misspelling, correct)
#> # A tibble: 455 x 3
#>    misspelling correct          n
#>    <chr>       <chr>        <int>
#>  1 abritrary   arbitrary        1
#>  2 accademic   academic         1
#>  3 accension   ascension        2
#>  4 accessable  accessible       1
#>  5 accidant    accident         1
#>  6 accidentaly accidentally     1
#>  7 accordeon   accordion        1
#>  8 addopt      adopt            1
#>  9 addtional   additional       1
#> 10 admendment  amendment        1
#> # ... with 445 more rows
```

So we found a match in the dictionary for about half of the misspellings. In how many of the ones we classified did we get at least one of our guesses right?


```r
which_correct <- joined %>%
  group_by(misspelling, correct) %>%
  summarize(guesses = n(), one_correct = any(correct == word))

which_correct
#> # A tibble: 455 x 4
#> # Groups:   misspelling [?]
#>    misspelling correct      guesses one_correct
#>    <chr>       <chr>          <int> <lgl>      
#>  1 abritrary   arbitrary          1 T          
#>  2 accademic   academic           1 T          
#>  3 accension   ascension          2 T          
#>  4 accessable  accessible         1 T          
#>  5 accidant    accident           1 T          
#>  6 accidentaly accidentally       1 F          
#>  7 accordeon   accordion          1 T          
#>  8 addopt      adopt              1 T          
#>  9 addtional   additional         1 T          
#> 10 admendment  amendment          1 T          
#> # ... with 445 more rows

# percentage of guesses getting at least one right
mean(which_correct$one_correct)
#> [1] 0.8527473

# number uniquely correct (out of the original 1000)
sum(which_correct$guesses == 1 & which_correct$one_correct)
#> [1] 294
```

Not bad.

#### Reporting distance in the joined output

If you wanted to include the distance as a column in your output, you can use the `distance_col` argument. For example, we may be interested in how many words were *two* letters apart.


```r
joined_dists <- sub_misspellings %>%
  stringdist_inner_join(words, by = c(misspelling = "word"), max_dist = 2,
                        distance_col = "distance")

joined_dists
#> # A tibble: 7,427 x 5
#>    misspelling correct    word       syllables distance
#>    <chr>       <chr>      <chr>          <dbl>    <dbl>
#>  1 charactors  characters character       3.00     2.00
#>  2 charactors  characters charactery      4.00     2.00
#>  3 sould       should     auld            1.00     2.00
#>  4 sould       should     bold            1.00     2.00
#>  5 sould       should     bound           1.00     2.00
#>  6 sould       should     cold            1.00     2.00
#>  7 sould       should     could           1.00     1.00
#>  8 sould       should     fold            1.00     2.00
#>  9 sould       should     foul            1.00     2.00
#> 10 sould       should     found           1.00     2.00
#> # ... with 7,417 more rows
```

Note the extra `distance` column, which in this case will always be less than or equal to 2. We could then pick the closest match for each, and examine how many of our closest matches were 1 or 2 away:


```r
closest <- joined_dists %>%
  group_by(misspelling) %>%
  top_n(1, desc(distance)) %>%
  ungroup()

closest
#> # A tibble: 1,437 x 5
#>    misspelling  correct      word        syllables distance
#>    <chr>        <chr>        <chr>           <dbl>    <dbl>
#>  1 charactors   characters   character        3.00     2.00
#>  2 charactors   characters   charactery       4.00     2.00
#>  3 sould        should       could            1.00     1.00
#>  4 sould        should       should           1.00     1.00
#>  5 sould        should       sold             1.00     1.00
#>  6 sould        should       soul             1.00     1.00
#>  7 sould        should       sound            1.00     1.00
#>  8 sould        should       would            1.00     1.00
#>  9 incorportaed incorporated incorporate      4.00     2.00
#> 10 awya         away         aa               2.00     2.00
#> # ... with 1,427 more rows

closest %>%
  count(distance)
#> # A tibble: 3 x 2
#>   distance     n
#>      <dbl> <int>
#> 1     0        1
#> 2     1.00   725
#> 3     2.00   711
```

#### Other joining functions

Note that `stringdist_inner_join` is not the only function we can use. If we're interested in including the words that we *couldn't* classify, we could have use `stringdist_left_join`:


```r
left_joined <- sub_misspellings %>%
  stringdist_left_join(words, by = c(misspelling = "word"), max_dist = 1)

left_joined
#> # A tibble: 1,273 x 4
#>    misspelling  correct      word   syllables
#>    <chr>        <chr>        <chr>      <dbl>
#>  1 charactors   characters   <NA>       NA   
#>  2 Brasillian   Brazilian    <NA>       NA   
#>  3 sould        should       could       1.00
#>  4 sould        should       should      1.00
#>  5 sould        should       sold        1.00
#>  6 sould        should       soul        1.00
#>  7 sould        should       sound       1.00
#>  8 sould        should       would       1.00
#>  9 belligerant  belligerent  <NA>       NA   
#> 10 incorportaed incorporated <NA>       NA   
#> # ... with 1,263 more rows

left_joined %>%
  filter(is.na(word))
#> # A tibble: 545 x 4
#>    misspelling  correct      word  syllables
#>    <chr>        <chr>        <chr>     <dbl>
#>  1 charactors   characters   <NA>         NA
#>  2 Brasillian   Brazilian    <NA>         NA
#>  3 belligerant  belligerent  <NA>         NA
#>  4 incorportaed incorporated <NA>         NA
#>  5 awya         away         <NA>         NA
#>  6 occuring     occurring    <NA>         NA
#>  7 surveilence  surveillance <NA>         NA
#>  8 abondoned    abandoned    <NA>         NA
#>  9 alledges     alleges      <NA>         NA
#> 10 deliberatly  deliberately <NA>         NA
#> # ... with 535 more rows
```

(To get *just* the ones without matches immediately, we could have used `stringdist_anti_join`). If we increase our distance threshold, we'll increase the fraction with a correct guess, but also get more false positive guesses:


```r
left_joined2 <- sub_misspellings %>%
  stringdist_left_join(words, by = c(misspelling = "word"), max_dist = 2)

left_joined2
#> # A tibble: 7,691 x 4
#>    misspelling correct    word       syllables
#>    <chr>       <chr>      <chr>          <dbl>
#>  1 charactors  characters character       3.00
#>  2 charactors  characters charactery      4.00
#>  3 Brasillian  Brazilian  <NA>           NA   
#>  4 sould       should     auld            1.00
#>  5 sould       should     bold            1.00
#>  6 sould       should     bound           1.00
#>  7 sould       should     cold            1.00
#>  8 sould       should     could           1.00
#>  9 sould       should     fold            1.00
#> 10 sould       should     foul            1.00
#> # ... with 7,681 more rows

left_joined2 %>%
  filter(is.na(word))
#> # A tibble: 264 x 4
#>    misspelling   correct       word  syllables
#>    <chr>         <chr>         <chr>     <dbl>
#>  1 Brasillian    Brazilian     <NA>         NA
#>  2 belligerant   belligerent   <NA>         NA
#>  3 occuring      occurring     <NA>         NA
#>  4 abondoned     abandoned     <NA>         NA
#>  5 correponding  corresponding <NA>         NA
#>  6 archeaologist archaeologist <NA>         NA
#>  7 emmediately   immediately   <NA>         NA
#>  8 possessess    possesses     <NA>         NA
#>  9 unahppy       unhappy       <NA>         NA
#> 10 Guilio        Giulio        <NA>         NA
#> # ... with 254 more rows
```

Most of the missing words here simply aren't in our dictionary.

You can try other distance thresholds, other dictionaries, and other distance metrics (see [stringdist-metrics] for more). This function is especially useful on a domain-specific dataset, such as free-form survey input that is likely to be close to one of a handful of responses.

### Example of `regex_inner_join`: Classifying text based on regular expressions

Consider the book Pride and Prejudice, by Jane Austen, which we can access through the [janeaustenr package](https://cran.r-project.org/package=janeaustenr).

We could split the books up into "passages" of 50 lines each.


```r
library(dplyr)
library(stringr)
library(janeaustenr)

passages <- data_frame(text = prideprejudice) %>%
  group_by(passage = 1 + row_number() %/% 50) %>%
  summarize(text = paste(text, collapse = " "))

passages
#> # A tibble: 261 x 2
#>    passage text                                                                                                                                        
#>      <dbl> <chr>                                                                                                                                       
#>  1    1.00 "PRIDE AND PREJUDICE  By Jane Austen    Chapter 1   It is a truth universally acknowledged, that a single man in possession of a good fortu…
#>  2    2.00 "\"How so? How can it affect them?\"  \"My dear Mr. Bennet,\" replied his wife, \"how can you be so tiresome! You must know that I am think…
#>  3    3.00 "are my old friends. I have heard you mention them with consideration these last twenty years at least.\"  \"Ah, you do not know what I suf…
#>  4    4.00 "herself, began scolding one of her daughters.  \"Don't keep coughing so, Kitty, for Heaven's sake! Have a little compassion on my nerves. …
#>  5    5.00 " The astonishment of the ladies was just what he wished; that of Mrs. Bennet perhaps surpassing the rest; though, when the first tumult of…
#>  6    6.00 "married, I shall have nothing to wish for.\"  In a few days Mr. Bingley returned Mr. Bennet's visit, and sat about ten minutes with him in…
#>  7    7.00 "introduced to any other lady, and spent the rest of the evening in walking about the room, speaking occasionally to one of his own party. …
#>  8    8.00 "party. Mr. Bingley had danced with her twice, and she had been distinguished by his sisters. Jane was as much gratified by this as her mot…
#>  9    9.00 "  Chapter 4   When Jane and Elizabeth were alone, the former, who had been cautious in her praise of Mr. Bingley before, expressed to her …
#> 10   10.0  Elizabeth listened in silence, but was not convinced; their behaviour at the assembly had not been calculated to please in general; and wit…
#> # ... with 251 more rows
```

Suppose we wanted to divide the passages based on which character's name is mentioned in each. Character's names may differ in how they are presented, so we construct a regular expression for each and pair it with that character's name.


```r
characters <- readr::read_csv(
"character,character_regex
Elizabeth,Elizabeth
Darcy,Darcy
Mr. Bennet,Mr. Bennet
Mrs. Bennet,Mrs. Bennet
Jane,Jane
Mary,Mary
Lydia,Lydia
Kitty,Kitty
Wickham,Wickham
Mr. Collins,Collins
Lady Catherine de Bourgh,de Bourgh
Mr. Gardiner,Mr. Gardiner
Mrs. Gardiner,Mrs. Gardiner
Charlotte Lucas,(Charlotte|Lucas)
")
```

Notice that for each character, we've defined a regular expression (sometimes allowing ambiguity, sometimes not) for detecting their name. Suppose we want to "classify" passages based on whether this regex is present.

With fuzzyjoin's `regex_inner_join` function, we do:


```r
character_passages <- passages %>%
  regex_inner_join(characters, by = c(text = "character_regex"))
```

This combines the two data frames based on cases where the `passages$text` column is matched by the `characters$character_regex` column. (Note that the dataset with the text column must always come first). This results in:


```r
character_passages %>%
  select(passage, character, text)
#> # A tibble: 1,126 x 3
#>    passage character       text                                                                                                                        
#>      <dbl> <chr>           <chr>                                                                                                                       
#>  1    1.00 Mr. Bennet      "PRIDE AND PREJUDICE  By Jane Austen    Chapter 1   It is a truth universally acknowledged, that a single man in possession…
#>  2    1.00 Jane            "PRIDE AND PREJUDICE  By Jane Austen    Chapter 1   It is a truth universally acknowledged, that a single man in possession…
#>  3    2.00 Mr. Bennet      "\"How so? How can it affect them?\"  \"My dear Mr. Bennet,\" replied his wife, \"how can you be so tiresome! You must know…
#>  4    2.00 Jane            "\"How so? How can it affect them?\"  \"My dear Mr. Bennet,\" replied his wife, \"how can you be so tiresome! You must know…
#>  5    2.00 Lydia           "\"How so? How can it affect them?\"  \"My dear Mr. Bennet,\" replied his wife, \"how can you be so tiresome! You must know…
#>  6    2.00 Charlotte Lucas "\"How so? How can it affect them?\"  \"My dear Mr. Bennet,\" replied his wife, \"how can you be so tiresome! You must know…
#>  7    3.00 Elizabeth       "are my old friends. I have heard you mention them with consideration these last twenty years at least.\"  \"Ah, you do not…
#>  8    3.00 Mr. Bennet      "are my old friends. I have heard you mention them with consideration these last twenty years at least.\"  \"Ah, you do not…
#>  9    3.00 Mrs. Bennet     "are my old friends. I have heard you mention them with consideration these last twenty years at least.\"  \"Ah, you do not…
#> 10    4.00 Mr. Bennet      "herself, began scolding one of her daughters.  \"Don't keep coughing so, Kitty, for Heaven's sake! Have a little compassio…
#> # ... with 1,116 more rows
```

This shows that Mr. Bennet's name appears in passages 1, 2, 4, and 6, while Charlotte Lucas's appears in 3. Notice that having fuzzy-joined the datasets, some passages will end up duplicated (those with multiple names in them), while it's possible others will be missing entirely (those without names).

We could ask which characters are mentioned in the most passages:


```r
character_passages %>%
  count(character, sort = TRUE)
#> # A tibble: 14 x 2
#>    character                    n
#>    <chr>                    <int>
#>  1 Elizabeth                  227
#>  2 Darcy                      159
#>  3 Jane                       134
#>  4 Mrs. Bennet                 89
#>  5 Wickham                     89
#>  6 Lydia                       79
#>  7 Mr. Collins                 75
#>  8 Charlotte Lucas             68
#>  9 Mr. Bennet                  55
#> 10 Kitty                       42
#> 11 Mrs. Gardiner               35
#> 12 Lady Catherine de Bourgh    25
#> 13 Mr. Gardiner                25
#> 14 Mary                        24
```

The data is also well suited to discover which characters appear in scenes together, and to cluster them to find groupings of characters (like in [this analysis](http://varianceexplained.org/r/love-actually-network/)).


```r
passage_character_matrix <- character_passages %>%
  group_by(passage) %>%
  filter(n() > 1) %>%
  reshape2::acast(character ~ passage, fun.aggregate = length, fill = 0)

passage_character_matrix <- passage_character_matrix / rowSums(passage_character_matrix)

h <- hclust(dist(passage_character_matrix, method = "manhattan"))

plot(h)
```

![plot of chunk character_passages_matrix](tools/README-character_passages_matrix-1.png)

Other options for further analysis of this fuzzy-joined dataset include doing sentiment analysis on text surrounding each character's name, [similar to Julia Silge's analysis here](http://juliasilge.com/blog/You-Must-Allow-Me/).

### Future Work

A few things I'd like to work on:

* **Shortcuts on string distance matching**: If two strings are more than 1 character apart in length, the method is `osa`, and `max_dist` is 1, you don't even need to compare them.

* **More examples**: I've used this package in other powerful ways, but on proprietary data. I'm interested in ideas for use cases that can be provided as vignettes.

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
