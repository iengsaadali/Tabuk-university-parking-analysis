# حساب نسبة الطلاب والساعات التشغيلية لكل قاعة ولكل يوم
analysis_by_day <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(ROOM_CODE, DAY_CODE) %>%
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),
    total_operating_hours = sum(operating_hours, na.rm = TRUE)
  )

# حساب الساعات التشغيلية ونسبة الطلاب لكل مبنى ولكل يوم
building_analysis_by_day <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(BULDING_DESC, DAY_CODE) %>%
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),
    total_operating_hours = sum(operating_hours, na.rm = TRUE)
  )



analysis_Students_Lectures <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(ROOM_CODE, DAY_CODE) %>%
  summarize(
    # حساب المتوسط اليومي لنسبة الطلاب
    average_student_percentage = mean(student_percentage, na.rm = TRUE),
    # حساب مجموع الساعات التشغيلية اليومية
    total_operating_hours = sum(operating_hours, na.rm = TRUE),
    .groups = 'drop'   )

install.packages("writexl")
write_xlsx(analysis_by_day, "C:/Users/Saad/Documents/analysis_by_day.xlsx")


analysis_by_building_day <- merged_new_table %>%
    select(BULDING_DESC, DAY_CODE, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>%  # تحديد الأعمدة المهمة فقط
    mutate(
        student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT ) * 100,
        operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
     ) %>%
     group_by(BULDING_DESC, DAY_CODE) %>%
     summarize(
         average_student_percentage = mean(student_percentage, na.rm = TRUE),
         total_operating_hours = sum(operating_hours, na.rm = TRUE),
         .groups = 'drop'
     )

final_aggregated_data <- analysis_by_building_day %>%
  group_by(BULDING_DESC, DAY_CODE) %>%
  summarize(
    final_average_student_percentage = mean(average_student_percentage, na.rm = TRUE),
    final_total_operating_hours = sum(total_operating_hours, na.rm = TRUE),
    .groups = 'drop'
  )


final_data_with_seats_students <- analysis_by_day %>%
  left_join(Student_Sche, by = c("ROOM_CODE", "DAY_CODE")) %>%
  left_join(room_seats_data_unique, by = "ROOM_CODE")

analysis_by_day <- merged_new_table %>%
  mutate(
    max_seat = ROOM_SEAT,  # عدد المقاعد الأقصى للقاعة
    enrolled_students = ENROLLED_STUDENTS  # عدد الطلاب المسجلين
  )



analysis_by_day <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(ROOM_CODE, DAY_CODE) %>%
  summarize(
    # حساب المتوسط لنسبة الطلاب
    average_student_percentage = mean(student_percentage, na.rm = TRUE),
    # جمع الساعات التشغيلية
    total_operating_hours = sum(operating_hours, na.rm = TRUE),
    # استخراج العدد الأقصى للمقاعد
    max_seat = first(ROOM_SEAT),
    # استخراج عدد الطلاب المسجلين
    total_enrolled_students = first(ENROLLED_STUDENTS)
  )


building_analysis_by_day <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(BULDING_DESC, DAY_CODE) %>%
  summarize(
    # حساب المتوسط لنسبة الطلاب
    average_student_percentage = mean(student_percentage, na.rm = TRUE),
    # جمع الساعات التشغيلية
    total_operating_hours = sum(operating_hours, na.rm = TRUE),
    # جمع العدد الأقصى للمقاعد في جميع القاعات
    total_max_seat = sum(ROOM_SEAT, na.rm = TRUE),
    # حساب العدد الإجمالي للطلاب المسجلين في المبنى لكل يوم
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE)
  )

# حساب العدد الإجمالي للمقاعد والطلاب لكل مبنى
building_analysis <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(BULDING_DESC) %>%
  summarize(
    # حساب المتوسط لنسبة الطلاب
    average_student_percentage = mean(student_percentage, na.rm = TRUE),
    # جمع الساعات التشغيلية
    total_operating_hours = sum(operating_hours, na.rm = TRUE),
    # حساب العدد الإجمالي للمقاعد في جميع القاعات للمبنى
    total_seat_capacity =max(ROOM_SEAT, na.rm = TRUE),  # هنا يتم جمع جميع المقاعد لكل مبنى
    # حساب العدد الإجمالي للطلاب المسجلين في المبنى
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE)
  )


building_analysis <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(BULDING_DESC, ROOM_CODE) %>%
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),  # حساب المتوسط لنسبة الطلاب
    total_operating_hours = sum(operating_hours, na.rm = TRUE),           # جمع الساعات التشغيلية
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),                   # جمع المقاعد لكل قاعة في المبنى
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE)        # جمع عدد الطلاب المسجلين لكل قاعة
  )


building_analysis <- merged_new_table %>%
  mutate(
    # حساب نسبة عدد الطلاب لكل محاضرة
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    # حساب عدد الساعات التشغيلية لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(BULDING_DESC) %>%  # تجميع حسب وصف المبنى فقط
  summarize(
    total_rooms = n(),  # عدد القاعات في المبنى
    average_student_percentage = mean(student_percentage, na.rm = TRUE),  # متوسط نسبة الطلاب
    total_operating_hours = sum(operating_hours, na.rm = TRUE),           # جمع الساعات التشغيلية
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),                   # جمع المقاعد لكل قاعة في المبنى
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE)        # جمع عدد الطلاب المسجلين لكل قاعة
  )


analysis_by_building_day <- merged_new_table %>%
  select(BULDING_DESC, DAY_CODE, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>%  # تحديد الأعمدة المهمة فقط
  mutate(
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(BULDING_DESC, DAY_CODE) %>%
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),  # متوسط نسبة الطلاب
    total_operating_hours = sum(operating_hours, na.rm = TRUE),           # جمع الساعات التشغيلية
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),                   # إجمالي المقاعد
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE),       # إجمالي عدد الطلاب المسجلين
    .groups = 'drop'
  )


analysis_by_building_day <- merged_new_table %>%
  select(BULDING_DESC, DAY_CODE, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>%  # تحديد الأعمدة المهمة فقط
  mutate(
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
  ) %>%
  group_by(BULDING_DESC, DAY_CODE) %>%
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),  # متوسط نسبة الطلاب
    total_operating_hours = sum(operating_hours, na.rm = TRUE),           # جمع الساعات التشغيلية
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),                   # إجمالي المقاعد
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE),       # إجمالي عدد الطلاب المسجلين
    total_classes = n(),                                                   # عدد المحاضرات في المبنى
    .groups = 'drop'
  )


write_xlsx(analysis_by_building_day, "C:/Users/Saad/Documents/analysis_by_building_day.xlsx")
write_xlsx(analysis_by_day, "C:/Users/Saad/Documents/analysis day lecture.xlsx")


analysis_building_day <- merged_new_table %>%
  select(BULDING_DESC, DAY_CODE, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>%  # تحديد الأعمدة المهمة فقط
  mutate(
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,  # نسبة عدد الطلاب لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))  # حساب الساعات التشغيلية لكل محاضرة
  ) %>%
  group_by(BULDING_DESC, DAY_CODE) %>%  # التجميع حسب المبنى واليوم
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),  # متوسط نسبة الطلاب لكل يوم في المبنى
    total_operating_hours = sum(operating_hours, na.rm = TRUE),           # إجمالي الساعات التشغيلية لكل يوم في المبنى
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),                   # إجمالي عدد المقاعد في المبنى (لكل يوم)
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE),       # إجمالي عدد الطلاب المسجلين في اليوم لكل مبنى
    total_classes = n(),                                                  # عدد المحاضرات في كل يوم لكل مبنى
    .groups = 'drop'
  )

analysis_by_building <- merged_new_table %>%
  select(BULDING_DESC, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>%  # تحديد الأعمدة المهمة فقط
  mutate(
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,  # نسبة عدد الطلاب لكل محاضرة
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))  # حساب الساعات التشغيلية لكل محاضرة
  ) %>%
  group_by(BULDING_DESC) %>%  # التجميع حسب المبنى فقط دون اعتبار الأيام
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),  # متوسط نسبة الطلاب لكل مبنى
    total_operating_hours = sum(operating_hours, na.rm = TRUE),           # إجمالي الساعات التشغيلية لكل مبنى
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),                   # إجمالي عدد المقاعد في المبنى
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE),       # إجمالي عدد الطلاب المسجلين لكل مبنى
    total_classes = n(),                                                  # عدد المحاضرات لكل مبنى
    .groups = 'drop'
  )

merged_new_table1$DAY_CODE <- as.factor(merged_new_table1$DAY_CODE)


analysis_by_building_day <- days_convert %>%
  select(BULDING_DESC, DAY_CODE, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>%  # تحديد الأعمدة المهمة فقط
  mutate(
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT ) * 100,
    operating_hours = as.numeric(difftime(END_TIME, START_TIME , units = "hours"))
  ) %>%
  group_by(BULDING_DESC, DAY_CODE) %>%  # تجميع على مستوى اليوم
  summarize(
    average_student_percentage = mean(student_percentage, na.rm = TRUE),
    total_operating_hours = sum(operating_hours, na.rm = TRUE),
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE), # جمع عدد المقاعد
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE), # جمع عدد الطلاب
    total_lectures = n(),  # عدد المحاضرات
    .groups = 'drop'
  )

merged_new_table1$DAY_CODE <- as.numeric(merged_new_table1$DAY_CODE)

merged_new_table1$DAY_CODE <- as.character(merged_new_table1$DAY_CODE)

merged_new_table1 <- merged_new_table1 %>%
  mutate(
    DAY_NAME = case_when(
      DAY_CODE == 1 ~ "Sunday",
      DAY_CODE == 2 ~ "Monday",
      DAY_CODE == 3 ~ "Tuesday",
      DAY_CODE == 4 ~ "Wednesday",
      DAY_CODE == 5 ~ "Thursday",
      DAY_CODE == 6 ~ "Friday",
      DAY_CODE == 7 ~ "Saturday",
      TRUE ~ NA_character_  # للتعامل مع القيم غير المتوقعة
    )
  )

write_xlsx(merged_new_table, "C:/Users/Saad/Documents/days_convert.xlsx")

fixing_mistakes <- fixing_mistakes %>%
  mutate(
    START_TIME = format(as.POSIXct(START_TIME, format = "%H:%M"), format = "%H:%M"),
    END_TIME = format(as.POSIXct(END_TIME, format = "%H:%M"), format = "%H:%M")
  )

fixing_mistakes_Ver_2 <- fixing_mistakes_Ver_2 %>%
  mutate(
    START_TIME = as.POSIXct(START_TIME, format = "%H:%M", tz = "UTC"),
    END_TIME = as.POSIXct(END_TIME, format = "%H:%M", tz = "UTC")
  )

building_capacity_analysis <- merged_new_table %>%
  group_by(BULDING_DESC) %>%
  summarize(
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE), # جمع عدد المقاعد لجميع القاعات في المبنى
    .groups = 'drop'
  )

building_capacity_analysis <- merged_new_table %>%
  select(BULDING_DESC, ROOM_SEAT) %>%  # تحديد الأعمدة المهمة
  distinct() %>%                       # الحصول على القيم الفريدة
  group_by(BULDING_DESC) %>%           # تجميع حسب اسم المبنى
  summarize(
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE), # جمع عدد المقاعد لجميع القاعات في المبنى
    .groups = 'drop'                     # إزالة التجميع
  )

building_capacity_analysis <- merged_new_table %>%
  group_by(BULDING_DESC) %>%                      # تجميع البيانات حسب اسم المبنى
  summarize(
    total_seat_capacity = sum(ROOM_SEAT), na.rm = TRUE),  # جمع عدد المقاعد الفريدة لجميع القاعات في المبنى
    num_rooms = n_distinct(ROOM_CODE),              # عدد القاعات الفريدة في كل مبنى
    .groups = 'drop'                                # إزالة التجميع
)

building_capacity_analysis <- merged_new_table %>%
  group_by(BULDING_DESC) %>%                      # تجميع البيانات حسب اسم المبنى
  summarize(
    total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),  # جمع عدد المقاعد لكل القاعات في المبنى
    num_rooms = n(),                                  # عدد القاعات في كل مبنى
    .groups = 'drop'                                  # إزالة التجميع
  )

building_capacity_analysis <- merged_new_table %>%
  group_by(BULDING_DESC, ROOM_CODE) %>%  # تجميع البيانات حسب وصف المبنى ورمز الوحدة
  summarize(
    total_seat_capacity = sum(unique(ROOM_SEAT), na.rm = TRUE),  # جمع عدد المقاعد الفريدة لكل القاعات في المبنى
    num_rooms = n(),  # عدد القاعات في كل مبنى
  )

building_capacity_analysis <- merged_new_table %>%
  group_by(BULDING_DESC) %>%                      # تجميع البيانات حسب اسم المبنى
  summarize(
    total_seat_capacity = sum(unique(ROOM_SEAT), na.rm = TRUE),  # جمع عدد المقاعد الفريدة لجميع القاعات في المبنى
    num_rooms = n_distinct(ROOM_CODE),              # عدد القاعات الفريدة في كل مبنى
    .groups = 'drop'                                # إزالة التجميع
  )

total_capacity_by_building <- merged_new_table %>%
  select(BULDING_DESC, ROOM_CODE, ROOM_SEAT) %>%
  distinct() %>%  
  group_by(BULDING_DESC) %>%
  summarize(total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE), .groups = 'drop')

building_analysis_NEW <- days_convert %>%
  select(BULDING_DESC, ROOM_CODE, ROOM_SEAT) %>%
  distinct() %>%  # الحصول على القيم الفريدة
  group_by(BULDING_DESC) %>%
  summarize(total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE), .groups = 'drop')
  select(BULDING_DESC, , ENROLLED_STUDENTS, START_TIME, END_TIME) %>%  # تحديد الأعمدة المهمة فقط
  mutate(
    operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours")),
    student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100
  ) %>%
  group_by(BULDING_DESC) %>%
  summarize(
    total_room_capacity = sum(ROOM_SEAT, na.rm = TRUE),  # إجمالي عدد المقاعد في المبنى
    total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE),  # إجمالي عدد الطلاب المسجلين في المبنى
    total_operating_hours = sum(operating_hours, na.rm = TRUE),  # إجمالي ساعات التشغيل في المبنى
    total_classes = n(),  # عدد المحاضرات في المبنى
    operating_percentage = (total_operating_hours / 11) * 100,  # نسبة ساعات التشغيل
    .groups = 'drop'
  )

  building_analysis_NEW <- days_convert %>%
    select(BULDING_DESC, ROOM_SEAT, ENROLLED_STUDENTS, START_TIME, END_TIME) %>%
    mutate(
      operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours")),
      operating_percentage = (sum(operating_hours) / (11 * n())) * 100  # نسبة التشغيل بالمقارنة مع الساعات المتاحة
    ) %>%
    group_by(BULDING_DESC) %>%
    summarize(
      total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),
      total_students_enrolled = sum(ENROLLED_STUDENTS, na.rm = TRUE),
      total_operating_hours = sum(operating_hours, na.rm = TRUE),
      operating_percentage = mean(operating_percentage, na.rm = TRUE),  # متوسط نسبة التشغيل
      .groups = 'drop'
    )

  building_analysis_NEW <- days_convert %>%
    select(BULDING_DESC, ROOM_CODE, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>% 
    mutate(
      student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,  # نسبة الطلاب لكل محاضرة
      operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))  # عدد ساعات التشغيل لكل محاضرة
    ) %>%
    group_by(BULDING_DESC) %>%
    summarize(
      total_seat_capacity = sum(ROOM_SEAT, na.rm = TRUE),  # إجمالي عدد المقاعد
      num_rooms = n_distinct(ROOM_CODE),  # عدد القاعات في المبنى
      total_operating_hours = sum(operating_hours, na.rm = TRUE),  # إجمالي ساعات التشغيل
      average_student_percentage = mean(student_percentage, na.rm = TRUE),  # متوسط نسبة الطلاب
      total_classes = n(),  # عدد المحاضرات في المبنى
      # حساب نسبة التشغيل (عدد الساعات التشغيلية لكل قاعة / 11 ساعات قصوى)
      operational_percentage = (total_operating_hours / (num_rooms * 55)) * 100,  # النسبة المئوية للتشغيل
      .groups = 'drop'
    )  

  building_analysis_final_8_10_ver2 <- fixing_mistakes_Ver_2 %>%
    select(BULDING_DESC, ROOM_CODE, ENROLLED_STUDENTS, ROOM_SEAT, START_TIME, END_TIME) %>% 
    mutate(
      student_percentage = (ENROLLED_STUDENTS / ROOM_SEAT) * 100,  # نسبة الطلاب لكل محاضرة
      operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))  # عدد ساعات التشغيل لكل محاضرة
    ) %>%
    group_by(BULDING_DESC) %>%
    summarize(
      total_rooms = n_distinct(ROOM_CODE),  # عدد القاعات في المبنى
      total_operating_hours = sum(operating_hours, na.rm = TRUE),  # إجمالي ساعات التشغيل
      average_student_percentage = mean(student_percentage, na.rm = TRUE),  # متوسط نسبة الطلاب
      total_classes = n(),  # عدد المحاضرات في المبنى
      total_enrolled_students = sum(ENROLLED_STUDENTS, na.rm = TRUE),  # إجمالي عدد الطلاب المسجلين
      operational_percentage_10H = (total_operating_hours / (total_rooms * 50)) * 100,  # النسبة المئوية للتشغيل
      operational_percentage_8H = (total_operating_hours / (total_rooms * 40)) * 100,
      .groups = 'drop'
    )  

  final_analysis_8_10_verB <- left_join(building_analysis_final_8_10_ver2, total_capacity_by_building, by = "BULDING_DESC")  
  
  write_xlsx(final_analysis_8_10_verB, "C:/Users/Saad/Documents/final analysis buildings.xlsx")


  lecture_times <- Student_Sche %>%
    select(ROOM_CODE, START_TIME, END_TIME) %>%  # تأكد من اختيار الأعمدة الصحيحة
    distinct()  # للحصول على القيم الفريدة    
  merged_data_hope <- left_join(days_convert, lecture_times, by = "ROOM_CODE")  
  
  fixing_mistakes_Ver_2 <- fixing_mistakes_Ver_2 %>%
    mutate(
      START_TIME = format(as.POSIXct(START_TIME, format = "%H:%M"), format = "%H:%M"),
      END_TIME = format(as.POSIXct(END_TIME, format = "%H:%M"), format = "%H:%M")
    )
  
  fixing_mistakes_Ver_2 <- fixing_mistakes_Ver_2 %>%
    mutate(
      START_TIME = as.POSIXct(START_TIME, format = "%H:%M"),  
      END_TIME = as.POSIXct(END_TIME, format = "%H:%M")       # تنسيق الساعة والدقيقة
    )
  
  
  daily_operating_percentage <- fixing_mistakes_Ver_2 %>%
    mutate(
      # حساب عدد الساعات التشغيلية لكل محاضرة
      operating_hours = as.numeric(difftime(END_TIME, START_TIME, units = "hours"))
    ) %>%
    group_by(BULDING_DESC, DAY_CODE) %>%
    summarize(
      operating_percentage_8_hours = (sum(operating_hours) / (n_distinct(ROOM_CODE) * 8)) * 100,
      operating_percentage_10_hours = (sum(operating_hours) / (n_distinct(ROOM_CODE) * 10)) * 100,
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = DAY_CODE, values_from = c(operating_percentage_8_hours, operating_percentage_10_hours),
                names_prefix = "day_")  
  
  write_xlsx(daily_operating_percentage, "C:/Users/Saad/Documents/daily_operating_percentage.xlsx")

  write_xlsx(attendance_by_time_block, "C:/Users/Saad/Documents/SDPANALYSIS.xlsx")
  
  merged_new_table1 <- merged_new_table1 %>%
    mutate(
      START_TIME = as.POSIXct(START_TIME, format = "%H:%M"),
      END_TIME = as.POSIXct(END_TIME, format = "%H:%M")
    )
  
  # Calculate attendance for each building, day, and time block
  attendance_by_time_block <- fixing_mistakes_Ver_2 %>%
    mutate(
      # Define the time block based on START_TIME
      time_block = case_when(
        START_TIME >= as.POSIXct("6:00", format = "%H:%M") & START_TIME < as.POSIXct("8:00", format = "%H:%M") ~ "6-8",
        START_TIME >= as.POSIXct("8:00", format = "%H:%M") & START_TIME < as.POSIXct("10:00", format = "%H:%M") ~ "8-10",
        START_TIME >= as.POSIXct("10:00", format = "%H:%M") & START_TIME < as.POSIXct("12:00", format = "%H:%M") ~ "10-12",
        START_TIME >= as.POSIXct("12:00", format = "%H:%M") & START_TIME < as.POSIXct("14:00", format = "%H:%M") ~ "12-14",
        START_TIME >= as.POSIXct("14:00", format = "%H:%M") & START_TIME < as.POSIXct("16:00", format = "%H:%M") ~ "14-16",
        START_TIME >= as.POSIXct("16:00", format = "%H:%M") & START_TIME < as.POSIXct("18:00", format = "%H:%M") ~ "16-18",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(BULDING_DESC, DAY_CODE, time_block) %>%  # Group by building, day, and time block
    summarize(
      total_students_present = sum(ENROLLED_STUDENTS, na.rm = TRUE),  # Sum students in each time block
      .groups = 'drop'
    ) %>%
    arrange(BULDING_DESC, DAY_CODE, time_block)
  
  fixing_mistakes_Ver_2 <- fixing_mistakes_Ver_2 %>%
    mutate(
      # Keep only the time part, removing the date
      START_TIME = format(as.POSIXct(START_TIME, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"),
      END_TIME = format(as.POSIXct(END_TIME, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
    )
  
  fixing_mistakes_Ver_2 <- fixing_mistakes_Ver_2 %>%
    mutate(
      START_TIME = format(as.POSIXct(START_TIME, format = "%H:%M"), format = "%H:%M"),
      END_TIME = format(as.POSIXct(END_TIME, format = "%H:%M"), format = "%H:%M")
    )
  
  fixing_mistakes_Ver_2 <- fixing_mistakes_Ver_2 %>%
    mutate(
      START_TIME = as.POSIXct(START_TIME, format = "%Y-%m-%d %H:%M:%S"),
      END_TIME = as.POSIXct(END_TIME, format = "%Y-%m-%d %H:%M:%S")
    ) 
  
  attendance_by_time_block <- fixing_mistakes_Ver_2 %>%
    mutate(
      START_TIME = format(as.POSIXct(START_TIME, format = "%H:%M:%S"), "%H:%M"),
      START_TIME = as.POSIXct(START_TIME, format = "%H:%M")
    ) %>%
    # Define the time block based on START_TIME
    mutate(
      time_block = case_when(
        START_TIME >= as.POSIXct("08:00", format = "%H:%M") & START_TIME < as.POSIXct("10:00", format = "%H:%M") ~ "08-10",
        START_TIME >= as.POSIXct("10:00", format = "%H:%M") & START_TIME < as.POSIXct("12:00", format = "%H:%M") ~ "10-12",
        START_TIME >= as.POSIXct("12:00", format = "%H:%M") & START_TIME < as.POSIXct("14:00", format = "%H:%M") ~ "12-14",
        START_TIME >= as.POSIXct("14:00", format = "%H:%M") & START_TIME < as.POSIXct("16:00", format = "%H:%M") ~ "14-16",
        START_TIME >= as.POSIXct("16:00", format = "%H:%M") & START_TIME < as.POSIXct("18:00", format = "%H:%M") ~ "16-18",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(time_block)) %>%  # Exclude rows
    group_by(BULDING_DESC, DAY_CODE, time_block) %>%  # Group by building, day, and time block
    summarize(
      total_students_present = sum(ENROLLED_STUDENTS, na.rm = TRUE),  # Sum students in each time block
      .groups = 'drop'
    ) %>%
    arrange(BULDING_DESC, DAY_CODE, time_block)
  
  install.packages("tidygeocoder")
  
  single_location <- data.frame(address = "جامعة تبوك, تبوك, السعودية")
  single_location <- single_location %>%
    geocode(address, method = "osm", verbose = TRUE)
  
  print(single_location)

  library(tidygeocoder)  
  library(dplyr)  
  
  data <- data.frame(
    address = "جامعة تبوك, تبوك, السعودية"
  )
  
  # البحث عن الإحداثيات وحفظ النتيجة في جدول جديد
  result <- data %>%
    geocode(address, method = "osm", verbose = TRUE)
  
  # عرض النتيجة للتأكد من الحصول على الإحداثيات
  print(result)
  
  
  
  
  
  
 
  group_8_10 <- T_TEST_SDP %>%
    filter(`Time Period` == "8:00-10:00") %>%
    select(College, Day, Precentage)
  
  group_other <- T_TEST_SDP %>%
    filter(`Time Period` != "8:00-10:00") %>%
    select(College, Day, Precentage)

  # T-Test 8-10 period vs other periods
  T_TEST_SDP <- t.test(group_8_10$Precentage, group_other$Precentage, 
                           alternative = "greater", var.equal = FALSE)
  
  # Display the T-Test results
  print("Overall T-Test results (8:00-10:00 vs other periods):")
  print(T_TEST_SDP)  
  
  T_TEST_SDP2$Time_Group <- ifelse(T_TEST_SDP2$`Time Period` == "8:00-10:00", "8:00-10:00", "Other")
  ggplot(T_TEST_SDP2, aes(x = Time_Group, y = Precentage, fill = Time_Group)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") +
    labs(title = "Average Occupancy Rate: 8:00-10:00 vs Other Periods",
         x = "Time Period",
         y = "Average Occupancy Rate (%)") +
    theme_minimal() +
    scale_fill_manual(values = c("8:00-10:00" = "dodgerblue", "Other" = "orange"))
  View(T_TEST_SDP)
  rlang::last_trace()
  write_xlsx(T_TEST_SDP2, "C:/Users/Saad/Documents/SDPTTEST.xlsX")
  
