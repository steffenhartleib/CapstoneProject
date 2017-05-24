maxNews = []

	
file = open('en_US.news.txt','r')
for i in file:
	length = len(i)
	maxNews.append(length)

maxBlog = []
file = open('en_US.blogs.txt','r')
for i in file:
	length = len(i)
	maxBlog.append(length)
	
print max(maxBlog)
print max(maxNews)



	