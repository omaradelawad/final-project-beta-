# استيراد الحزم المطلوبة
library(arules)
library(readxl)

# تحميل البيانات من ملف Excel


# تقسيم العناصر في عمود 'items' إلى قوائم
item_list <- strsplit(as.character(data$items), ",")

# تحويل القوائم إلى كائن من النوع 'transactions'
transactions <- as(item_list, "transactions")

# تطبيق خوارزمية apriori
rules <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.1))

# عرض القواعد المكتشفة
inspect(rules)

# رسم تردد العناصر الأكثر شيوعًا
itemFrequencyPlot(transactions, topN = 5, type = "relative")