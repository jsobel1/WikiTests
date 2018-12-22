# #' Get all edits in a sorted table of wikipedien english in a given time interval.
# #'
# #' @param currentdate Current date in string (i.e. 20181104000000)
# #' @param pastdate Past date in string (i.e. 20181104000000)
# #' @return Sorted table of the number of edits/reverts per wikipedia articles in a given time interval
# #'
# #' @export
# #' @examples
# #' get_All_article_nb_edits_full_day("20181103010000","20181103000000")
#
# get_All_article_nb_edits_full_day=function(currentdate,pastdate){
#
#   output_table=c()
#   cmd=paste("https://en.wikipedia.org/w/api.php?action=query&list=allrevisions&arvprop=ids|timestamp|flags|comment|user|size|tags&arvdir=older&arvlimit=max&format=json&arvend=",pastdate,"&arvstart=",currentdate,sep="")
#   resp=httr::GET(cmd)
#   parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
#
#   get_nb_rev=function(rev){return(
#     dim(dplyr::filter(rev,tags %in% c("mw-undo","mv-rollback")))[1])}
#
#   output_table=data.frame(art=parsed$query$allrevisions$title,nb_edits=sapply(parsed$query$allrevisions$revisions,get_nb_rev))
#
#   while(length(parsed$continue$arvcontinue)==1){
#     output_table_load=c()
#    print(parsed$continue$arvcontinue)
#     rvc=parsed$continue$arvcontinue
#     cmd=paste("https://en.wikipedia.org/w/api.php?action=query&list=allrevisions&arvprop=ids|timestamp|flags|comment|user|size|tags&arvdir=older&arvlimit=max&format=json&arvend=",pastdate,"&arvstart=",currentdate,"&arvcontinue=",rvc,sep="")
#     resp=httr::GET(cmd)
#     parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
#     output_table_load=cbind(art=parsed$query$allrevisions$title,nb_edits=sapply(parsed$query$allrevisions$revisions,get_nb_rev))
#
#     output_table=try(rbind(output_table,output_table_load),silent=T)
#   }
#   output_table$nb_edits=as.numeric(as.character(output_table$nb_edits))
#   output_table=output_table%>% dplyr::group_by(art)%>% dplyr::summarise(sum_nb_edits=sum(nb_edits))%>%dplyr::arrange(desc(sum_nb_edits))%>%dplyr::filter(sum_nb_edits>0)%>%data.frame()
#   output_table=cbind(output_table,date=rep(substr(args[2], 1, 8),dim(output_table)[1]))
#   return(output_table)
# }

#' Get all edits in a sorted table of wikipedien english for a given article name.
#'
#' @param article_name Name of wikipedia article in string (i.e. Circadian clock)
#' @return full edits history of the wikipedia article given as input.
#'
#' @export
#' @examples
#' get_article_full_history_table("Jeffrey C. Hall")

get_article_full_history_table=function(article_name){
  what="ids|timestamp|comment|user|userid|size|content" #|parsedcomment|tags|flags
  #article_name="Circadian rhythm"
  article_name_c=gsub(" ","%20",article_name)
  output_table=c()
  cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=01012001&rvdir=newer&format=json&rvlimit=max",sep="")
  resp=httr::GET(cmd)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tt=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
  name_tab=rep(article_name,dim(eval(parse(text=tt)))[1])
  tmp_tab=eval(parse(text=tt))
  output_table=cbind(art=name_tab,tmp_tab[,c("revid","parentid","user","userid","timestamp","size","comment","*")])


  while(length(parsed$continue$rvcontinue)==1){
    output_table_load=c()
    print(parsed$continue$rvcontinue)
    rvc=parsed$continue$rvcontinue
    cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=01012001&rvdir=newer&format=json&rvlimit=max&rvcontinue=",rvc,sep="")
    resp=httr::GET(cmd)
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
    tt2=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
    name_tab2=rep(article_name,dim(eval(parse(text=tt2)))[1])

    tmp_tab2=eval(parse(text=tt2))

    #print(length(name_tab2))
    if(length(name_tab2)<1) break
    output_table_load=cbind(art=name_tab2,tmp_tab2[,c("revid","parentid","user","userid","timestamp","size","comment","*")])
    #print(dim(output_table))
    #print(dim(output_table_load))
    #print(colnames(output_table_load))

    output_table=try(rbind(output_table,output_table_load),silent=T)
  }
  return(output_table)
}


# get_temporal_profile=function(name_article){
#   donald_time=wiki_reverts_trend%>%
#     dplyr::filter(art==name_article)%>%data.frame()
#
#   donald_time$tsc=as.Date(ymd(donald_time$date))
#
#   donald_time$cut=cut( donald_time$tsc, breaks="1 week")
#
#   donald_time_month=donald_time%>%dplyr::group_by(cut)%>%summarise(freq=sum(nb_reverts))%>%data.frame()
#
#   P=ggplot(donald_time_month, aes(x =as.Date(cut),y=freq)) +scale_x_date(date_breaks = "1 month",date_labels = "%b" )+
#     geom_line(stat="identity",size=1.2,color="red")+labs(x="Month (2018)",y="Reverts per week")+
#     ggtitle(paste(name_article," weekly profile"))+theme_classic()+theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
#   print(P)
#
# }

