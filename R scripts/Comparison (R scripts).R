#Frequency of each score in Prepaid Card score distribution
count(Consumer_Complaint_Narratives.scores$score)

#Frequency of each score in Student Loan score distribution
count(Student_Complaint_Narratives.scores$score)

#Compare prepaid card complaints to student loan complaints datasets              
all.scores = rbind(Consumer_Complaint_Narratives.scores, Student_Complaint_Narratives.scores)
ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_histogram(mapping=aes(x=score, fill=Feedback), binwidth=0.5) +
  facet_grid(Feedback~.) + # make a separate plot for each hashtag
  theme_bw(base_size = 12) + scale_fill_brewer(palette = 18) # plain display, nicer colors
