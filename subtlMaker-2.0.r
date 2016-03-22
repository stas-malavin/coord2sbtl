subtlMaker <- function(infile = file.choose(),
	outfile = paste0(substr(infile, 1, nchar(infile)-3), 'srt'),
	start = '00:00:00',
	finish = as.POSIXlt(length(q), origin = Sys.Date(), tz = 'GMT'))
	# time1 and time2, if specified, should be strings in 'hh:mm:ss' format
{
	q <- readLines(infile)
	start <- as.POSIXlt(start, format = '%H:%M:%S') + 23
	finish <- as.POSIXlt(finish, format = '%H:%M:%S') + 23
	q <- q[grep(format(start,'%H:%M:%S'), q):grep(format(finish,'%H:%M:%S'), q)]
	for (i in 1:length(q)) {
		t <- as.POSIXlt((i-1), origin = Sys.Date(), tz = 'GMT')
		q[i] <- sub('^\\d{2}:\\d{2}:\\d{2},',
			paste0(	i-1,'\n',format(t,'%H:%M:%OS3'),' --> ',format(t+1,'H:%M:%OS3\nE')), q[i])
		q[i] <- sub('$', '\n', q[i])
		q[i] <- sub(',', ' N', q[i])
	}
	write(q, outfile)
}
