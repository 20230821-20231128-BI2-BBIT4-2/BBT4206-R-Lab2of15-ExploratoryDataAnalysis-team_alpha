if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

if (!is.element("readr", installed.packages()[, 1])) {
  install.packages("readr", dependencies = TRUE)
}
require("readr")

##load dataset
student_performance_dataset <-
  readr::read_csv(
    "data/Student_Performance_Dataset.csv", # nolint
    col_types =
      readr::cols(
        class_group =
          readr::col_factor(levels = c("A", "B", "C")),
        gender = readr::col_factor(levels = c("1", "0")),
        YOB = readr::col_date(format = "%Y"),
        regret_choosing_bi =
          readr::col_factor(levels = c("1", "0")),
        drop_bi_now =
          readr::col_factor(levels = c("1", "0")),
        motivator =
          readr::col_factor(levels = c("1", "0")),
        read_content_before_lecture =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        anticipate_test_questions =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        answer_rhetorical_questions =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        find_terms_I_do_not_know =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        copy_new_terms_in_reading_notebook =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        take_quizzes_and_use_results =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        reorganise_course_outline =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        write_down_important_points =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        space_out_revision =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        studying_in_study_group =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        schedule_appointments =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        goal_oriented =
          readr::col_factor(levels =
                              c("1", "0")),
        spaced_repetition =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        testing_and_active_recall =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        interleaving =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        categorizing =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        retrospective_timetable =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        cornell_notes =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        sq3r = readr::col_factor(levels =
                                   c("1", "2", "3", "4")),
        commute = readr::col_factor(levels =
                                      c("1", "2",
                                        "3", "4")),
        study_time = readr::col_factor(levels =
                                         c("1", "2",
                                           "3", "4")),
        repeats_since_Y1 = readr::col_integer(),
        paid_tuition = readr::col_factor(levels =
                                           c("0", "1")),
        free_tuition = readr::col_factor(levels =
                                           c("0", "1")),
        extra_curricular = readr::col_factor(levels =
                                               c("0", "1")),
        sports_extra_curricular =
          readr::col_factor(levels = c("0", "1")),
        exercise_per_week = readr::col_factor(levels =
                                                c("0", "1",
                                                  "2",
                                                  "3")),
        meditate = readr::col_factor(levels =
                                       c("0", "1",
                                         "2", "3")),
        pray = readr::col_factor(levels =
                                   c("0", "1",
                                     "2", "3")),
        internet = readr::col_factor(levels =
                                       c("0", "1")),
        laptop = readr::col_factor(levels = c("0", "1")),
        family_relationships =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        friendships = readr::col_factor(levels =
                                          c("1", "2", "3",
                                            "4", "5")),
        romantic_relationships =
          readr::col_factor(levels =
                              c("0", "1", "2", "3", "4")),
        spiritual_wellnes =
          readr::col_factor(levels = c("1", "2", "3",
                                       "4", "5")),
        financial_wellness =
          readr::col_factor(levels = c("1", "2", "3",
                                       "4", "5")),
        health = readr::col_factor(levels = c("1", "2",
                                              "3", "4",
                                              "5")),
        day_out = readr::col_factor(levels = c("0", "1",
                                               "2", "3")),
        night_out = readr::col_factor(levels = c("0",
                                                 "1", "2",
                                                 "3")),
        alcohol_or_narcotics =
          readr::col_factor(levels = c("0", "1", "2", "3")),
        mentor = readr::col_factor(levels = c("0", "1")),
        mentor_meetings = readr::col_factor(levels =
                                              c("0", "1",
                                                "2", "3")),
        Attendance Waiver Granted: 1 = Yes, 0 = No =
          readr::col_factor(levels = c("0", "1")),
        GRADE = readr::col_factor(levels =
                                    c("A", "B", "C", "D",
                                      "E"))),
    
    locale = readr::locale())

View(student_performance_dataset)

summary(student_performance_dataset)

dim(student_performance_dataset)
sapply(student_performance_dataset, class)

student_performance_dataset_freq <- student_performance_dataset$chas
cbind(frequency = table(student_performance_dataset_freq),
      percentage = prop.table(table(student_performance_dataset_freq)) * 100)

student_performance_dataset_chas_mode <- names(table(student_performance_dataset$chas))[
  which(table(student_performance_dataset$chas) == max(table(student_performance_dataset$chas)))
]
print(student_performance_dataset_chas_mode)

summary(student_performance_dataset)

sapply(student_performance_dataset[, c(97,78,79,81,82,86,91,92,96,99)], sd)

student_performance_dataset_cov <- cov(student_performance_dataset[,c(97,78,79,81,82,86,91,92,96,99),])
View(student_performance_dataset_cov)

student_performance_dataset_cor <- cor(student_performance_dataset[, c(97,78,79,81,82,86,91,92,96,99),])
View(student_performance_dataset_cor)


student_performance_dataset_one_way_anova <- aov(TOTAL = Coursework TOTAL + EXAM (100%)   ~ studying_in_study_group, data = student_performance_dataset)
summary(student_performance_dataset_one_way_anova)

sapply(student_performance_dataset, class)



student_performance_dataset_additive_two_way_anova <- aov(TOTAL = Coursework TOTAL + EXAM (100%) ~ studying_in_study_group + space_out_revision, # nolint
                                                          data = student_performance_dataset)
summary(student_performance_dataset_additive_two_way_anova)



student_performance_dataset_interactive_two_way_anova <- aov(TOTAL = Coursework TOTAL + EXAM (100%) ~ studying_in_study_group * space_out_revision, # nolint
                                                             data = student_performance_dataset)
summary(student_performance_dataset_interactive_two_way_anova)



boxplot(student_performance_dataset[, 96], main = names(student_performance_dataset)[96])



barplot(table(student_performance_dataset[, 96]), main = names(student_performance_dataset)[96])


if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE)
}
require("Amelia")

missmap(student_performance_dataset, col = c("red", "grey"), legend = TRUE)



if (!is.element("corrplot", installed.packages()[, 1])) {
  install.packages("corrplot", dependencies = TRUE)
}
require("corrplot")
corrplot(cor(student_performance_dataset[, 96:97]), method = "circle")


if (!is.element("ggcorrplot", installed.packages()[, 1])) {
  install.packages("ggcorrplot", dependencies = TRUE)
}
require("ggcorrplot")
ggcorrplot(cor(student_performance_dataset[, 96:97]))

sapply(student_performance_dataset[, c(97,78,79,81,82,86,91,92,96,99)], var)
if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

sapply(student_performance_dataset[,c(97,78,79,81,82,86,91,92,96,99)],  kurtosis, type = 2)