# tinh bmi
bmi.function <- function(height, weight){
  bmi <- weight/(height * height)
  # print(bmi)
  return(bmi)
}

danh_gia_bmi.function <- function(bmi){
  result <-""
  if(bmi < 18.5){
    result <- "Gay"
  }else if(bmi<25){
    result <- "Binh thuong"
  }else{
    result <- "Thua can"
  }
  return(result)
}

# giai PT bac 1
ptb1.function <- function(a,b){
  result <- ""
  if(a==0 & b!=0){
    result <- "PT vo nghiem"
  }else if(a==0 & b==0){
    result <- "PT vo so nghiem"
  }else{
    result <- paste("Nghiem:", -b/a)
  }
  return(result)
}


# giai PT bac 2
ptb2.function <- function(a,b,c){
  result <- ""
  # khi a == 0 => giai PTB1
  if(a==0){
    result <- ptb1.function(b,c)
  }else{
  # giai PTB2
    delta <- b*b - 4*a*c
    if(delta<0){
      result <- "Phuong trinh vo nghiem"
    } else if(delta==0){
      nghiem <- -b/(2*a)
      result <- paste("x1 = x2 = x0 =", nghiem)
    } else{
      x1 <- (-b + sqrt(delta))/(2*a)
      x2 <- (-b - sqrt(delta))/(2*a)
      result <- paste("x1 =", x1, ", x2 =", x2)
    }
  }
  return(result)
}

# functions cho chuong 4
# tinh tien dien
tinh_tien_dien.function <- function(so_kw){
  # tinh tien dien cho ho gia dinh dua theo bieu gia
  muc1 = 1484
  muc2 = 1533
  muc3 = 1786
  muc4 = 2242
  muc5 = 2503
  muc6 = 2587
  
  bac50 = 50
  bac100 = 100
  
  tien_dien = 0
  
  if (so_kw <= 50){
    tien_dien = so_kw * muc1
  } else if(so_kw <= 100){
    tien_dien = bac50 * muc1 + (so_kw - bac50) * muc2
  } else if(so_kw <= 200){
    tien_dien = bac50 * muc1 + bac50 * muc2 + (so_kw - bac100) * muc3 
  } else if (so_kw <= 300){
    tien_dien = bac50 * muc1 + bac50 * muc2 + bac100 * muc3 + 
      (so_kw - bac50 - bac50 - bac100) * muc4
  } else if (so_kw <= 400){
    tien_dien = bac50 * muc1 + bac50 * muc2 + bac100 * muc3 + 
      bac100 * muc4 + (so_kw - bac50 - bac50 - bac100 - bac100) * muc5
  } else{
    tien_dien = bac50 * muc1 + bac50 * muc2 + bac100 * muc3 + 
      bac100 * muc4 + bac100 * muc5 + (so_kw - bac50 - bac50 - bac100 - bac100 - bac100) * muc6
  }
  return(tien_dien)
}

# kiem tra so nguyen to
kt_soNT.function <- function(x){
  i <- 1
  count <- 0
  while (i<=x){
    if(x%%i==0){
      count <- count+1
    }
    i <- i+1
  }
  return(count==2)
}

