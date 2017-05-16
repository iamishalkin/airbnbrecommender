login="smth@gmail.com"
password=""

get_uid=function(login,password){
command=paste('python login.py ',login, ' ', password)

uid=system(command, wait=FALSE, intern = T)
wrong=TRUE
invisible(try(
if(grepl("\\d", uid)){
  wrong=FALSE
  }, T))

if (wrong){
  print('Неверный логин или пароль')
  return(0)
} else {
  print('Ваш ID ')
  print(uid)
  return(uid)
}
}

a=get_uid(login, password )
