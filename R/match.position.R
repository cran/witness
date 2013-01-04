match.position <-
function(vector.match, pos, position = TRUE){
	v = order(vector.match, decreasing = TRUE)
	if (position){
		return(v[pos])
	} else {
		return(vector.match[v][pos])
	}
}
