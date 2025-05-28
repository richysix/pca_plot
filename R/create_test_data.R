library('tidyverse')
library('rprojroot')

root_path <- find_root(is_rstudio_project)
somite_numbers <- 17:25
num_clutches <- 2
per_clutch <- 9
num_genotypes <- 3
num_stages <- range(somite_numbers)[2] - range(somite_numbers)[1] + 1
num_samples <- num_clutches * per_clutch
set.seed(754)
test_sample_data <- tibble(
  sample_name = paste('sample', seq_len(num_samples), sep = '-'),
  clutch = factor(rep(paste0('clutch', 1:2), each = per_clutch)),
  genotype = factor(rep(c('wt', 'het', 'hom'), each = num_genotypes, num_clutches),
                    levels = c('wt', 'het', 'hom')),
  somiteNumber = sample(somite_numbers, num_samples, replace = TRUE),
  stage = factor(paste0(somiteNumber, '-somites'),
                 levels = paste0(somite_numbers, '-somites')),
  diverging_scale = rnorm(n = num_samples)
) %>% arrange(., somiteNumber, clutch)

# make PC4 proportional to somite number
slope <- (36 - -35 + 1) / num_stages
intercept <- -35

test_pca_data <- tibble(
  sample_name = test_sample_data$sample_name,
  PC1 = ifelse(test_sample_data$clutch == "clutch1", 
               sample(seq(-35,-20), 18, replace = TRUE),
               sample(seq(20,35), 18, replace = TRUE)),
  PC2 = ifelse(test_sample_data$clutch == "clutch-1", 
               sample(seq(-35,-20), 18, replace = TRUE),
               sample(seq(20,35), 18, replace = TRUE)),
  PC3 = case_when(
    test_sample_data$genotype == "wt" ~ sample(seq(-35,-20), 18, replace = TRUE),
    test_sample_data$genotype == "het" ~ sample(seq(-10,10), 18, replace = TRUE),
    test_sample_data$genotype == "hom" ~ sample(seq(20,35), 18, replace = TRUE),
  ),
  PC4 = ((test_sample_data$somiteNumber - somite_numbers[1]) * slope) + intercept
) %>% 
  mutate(., across(.cols = starts_with('PC'), ~ .x + runif(num_samples, min = -1, max = 1))) # add some jitter

write_tsv(test_sample_data, file.path('inst', 'extdata', 'test-samples.txt'))
write_tsv(test_pca_data, file.path('inst', 'extdata', 'test-pca.tsv'))

combined_data <- inner_join(test_sample_data, test_pca_data)
save(combined_data, file = file.path(root_path, 'R', 'sysdata.rda'))
