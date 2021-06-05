if(!require(dplyr)){
    install.packages("dplyr")
}
library(dplyr)

if(!require(lubridate)){
    install.packages("lubridate")
}
library(lubridate)

if(!require(ggplot2)){
    install.packages("ggplot2")
}
library(ggplot2)

if(!require(shiny)){
    install.packages("shiny")
}
library(shiny)
if(!require(ggrepel)){
    install.packages("ggrepel")
}
library(ggrepel)
if(!require(data.table)){
    install.packages("data.table")
}
library(data.table)
if(!require(FSA)){
    install.packages("FSA")
}
library(FSA)
ui <- fluidPage(
    list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   width: 350px;
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
    )),
    # Application title
    titlePanel("COVID-19　神奈川県内の感染状況"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput("date1"),
            h5("プロットを行う日付の最大値"),
            h5("または先週比の基準となる日付"),
            uiOutput("date2"),
            h5("該当する市区町村にチェックしてください。"),
            fluidRow(column(width = 5,
                            tags$div(align = 'left', 
                     class = 'multicol', 
                     list(
                     checkboxGroupInput("pre",NULL,
                               #width='250px',
                               choices=,c(
                                   "横浜市"="横浜市",
                                   "川崎市川崎区"="川崎市川崎区",
                                   "川崎市幸区"="川崎市幸区",
                                   "川崎市中原区"="川崎市中原区",
                                   "川崎市高津区"="川崎市高津区",
                                   "川崎市多摩区"="川崎市多摩区",
                                   "川崎市宮前区"="川崎市宮前区",
                                   "川崎市麻生区"="川崎市麻生区",
                                   "相模原市"="相模原市",
                                   "横須賀市"="横須賀市",
                                   "平塚市"="平塚市",
                                   "鎌倉市"="鎌倉市",
                                   "藤沢市"="藤沢市",
                                   "小田原市"="小田原市",
                                   "茅ヶ崎市"="茅ヶ崎市",
                                   "逗子市"="逗子市",
                                   "三浦市"="三浦市",
                                   "秦野市"="秦野市",
                                   "厚木市"="厚木市",
                                   "大和市"="大和市",
                                   "伊勢原市"="伊勢原市",
                                   "海老名市"="海老名市",
                                   "座間市"="座間市",
                                   "南足柄市"="南足柄市",
                                   "綾瀬市"="綾瀬市",
                                   "葉山町"="葉山町",
                                   "寒川町"="寒川町",
                                   "大磯町"="大磯町",
                                   "二宮町"="二宮町",
                                   "中井町"="中井町",
                                   "大井町"="大井町",
                                   "松田町"="松田町",
                                   "山北町"="山北町",
                                   "開成町"="開成町",
                                   "箱根町"="箱根町",
                                   "真鶴町"="真鶴町",
                                   "湯河原町"="湯河原町",
                                   "愛川町"="愛川町",
                                   "清川村"="清川村"
                                         )))))),
            ),
        

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("感染者数",plotOutput("line5")),
                        tabPanel("10万人当たりの累積感染者数",plotOutput("line")),
                        tabPanel("先週比",
                                 h5("入力変数は2つ目の日付と都道府県のみ"),
                                 fluidRow(
                                     column(6,
                                            numericInput("num1","x軸の最大値の設定",
                                              value=60)),
                                     column(6,
                                            numericInput("num2","y軸の最大値の設定",
                                              value=100)
                                            )),
                                 plotOutput("line2")),
                        tabPanel("10万人当たりの感染者数と増加率",
                                 fluidRow(
                                     column(6,
                                            numericInput("num3","x軸の最大値の設定",
                                                         value=60)),
                                     column(6,
                                            numericInput("num4","y軸の最大値の設定",
                                                         value=1.5))),
                                 plotOutput("line3")),
                        tabPanel("増加率",
                                 fluidRow(
                                     column(6,
                                            numericInput("num5","x軸の最大値の設定",
                                                         value=1.5)),
                                     column(6,
                                            numericInput("num6","y軸の最大値の設定",
                                                         value=1.5))),
                                 plotOutput("line4")))
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data<-
        fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/coviddata.csv",encoding = "UTF-8")%>%
        filter(!is.na(Fixed_Date))%>%
        mutate(Fixed_Date=as.Date(Fixed_Date))
    jinko<-
        read.csv("jinko.csv",encoding = "SJIS")
    date1<-
        data%>%
        data.frame()%>%
        distinct(Fixed_Date)%>%
        arrange(desc(Fixed_Date))%>%
        distinct(Fixed_Date)
    date2<-
        data%>%
        data.frame()%>%
        distinct(Fixed_Date)%>%
        arrange(Fixed_Date)%>%
        distinct(Fixed_Date)
    output$date1<-
        renderUI({
            dateInput("date1",
                      label = h5("プロットを行う日付の最小値"),
                      min = date2[1,1],
                      max = date1[1,1],
                      value = as.Date("2020-07-06"))
        })
    output$date2<-
        renderUI({
            dateInput("date2",
                      label = NULL,
                      min = date2[1,1],
                      max = date1[1,1],
                      value = date1[1,1])
        })
    # days<-
    #     reactive({
    #         data%>%
    #             distinct(Fixed_Date,Residential_City)%>%
    #             mutate(
    #                 wday1=lubridate::wday(Fixed_Date,label = TRUE),
    #                 wday2=lubridate::wday(input$date2,label = TRUE),
    #                 #wday2=lubridate::wday("2021-05-29",label = TRUE),
    #                    wd_flag=(wday1==wday2),
    #                 diff=input$date1-Fixed_Date
    #                 #diff=as.Date("2021-05-29")-Fixed_Date
    #                 )%>%
    #             filter(wd_flag==T)%>%
    #             #arrange(desc(Fixed_Date))%>%
    #             group_by(Residential_City)%>%
    #             mutate(rank=min_rank(desc(diff)))%>%
    #             filter(rank==1)%>%
    #             ungroup()
    #             
    #     })
    flag<-
        reactive({
            data.frame(date=as.Date(input$date1))%>%
                #data.frame(date=as.Date("2020-04-21"))%>%
                mutate(wday=lubridate::wday(date),
                       wday2=lubridate::wday(input$date2,label = TRUE),
                       #wday2=lubridate::wday("2021-05-29"),
                       diff=as.numeric(wday2)-as.numeric(wday),
                       date2=date+diff)
        })
    data2<-
        reactive({
            
                data%>%
                count(Fixed_Date,Residential_City)%>%
                #filter(Fixed_Date<days()$Fixed_Date,Fixed_Date>=input$date1)%>%
                filter(Fixed_Date<=input$date2,Fixed_Date>=flag()$date2)%>%
                tidyr::complete(Fixed_Date=tidyr::full_seq(Fixed_Date,1),Residential_City,fill = list(n = 0))%>%
                arrange(desc(Fixed_Date))%>%
                #arrange(Fixed_Date)%>%
                group_by(Residential_City)%>%
                mutate(flag1=1,
                       flag2=cumsum(flag1),
                       flag3=(flag2-1)%/%7+1,#週番号
                       flag4=flag2-7*flag3
                       #flag4=(flag2-1)-(flag3-1)*7
                       )%>%
                mutate(Date=Fixed_Date+flag4)%>%
                #mutate(Date=Fixed_Date-flag4)%>%#市区町村ごとに最初の日付を計算
                ungroup()%>%
                mutate(year=year(Fixed_Date))
        })
        
        
        # mutate(wday=lubridate::wday(Fixed_Date,label = TRUE),
        #        week=strftime(Fixed_Date,"%V"),
        #        year=year(Fixed_Date),
        #        wday2=lubridate::wday(Fixed_Date))%>%
        # mutate(Date=Fixed_Date-lubridate::wday(Fixed_Date)+2)
    data3<-
        reactive({
            left_join(data2(),jinko,by=c("Residential_City"="City"))
        })
            

    data4<-
        reactive({
            data3()%>%
                filter(!Residential_City%in%c("鎌倉保健福祉事務所",
                                              "厚木保健福祉事務所",
                                              "平塚保健福祉事務所",
                                              "小田原保健福祉事務所",
                                              "茅ヶ崎保健所",
                                              "茅ヶ崎市保健所"))%>%
                filter(!is.na(jinko))%>%
                group_by(Date,Residential_City,year,jinko)%>%
                summarise(count=sum(n))%>%
                mutate(count_j=count/jinko*100000)%>%
                filter(!is.na(count_j))%>%
                ungroup()
        })
        


    data5<-
        data%>%
        left_join(jinko,by=c("Residential_City"="City"))%>%
        filter(!Residential_City%in%c("鎌倉保健福祉事務所",
                                      "厚木保健福祉事務所",
                                      "平塚保健福祉事務所",
                                      "小田原保健福祉事務所",
                                      "茅ヶ崎保健所",
                                      "茅ヶ崎市保健所"))
        
     output$line5<-
        renderPlot({
            data4()%>%
                filter(Residential_City%in%input$pre)%>%
                ggplot2::ggplot(aes(x=Date,y=count,color=Residential_City))+
                ggplot2::geom_line()+
                labs(y="感染者数",
                     color="市区町村")+
                # scale_x_continuous(breaks=seq(flag()$date2,as.Date(input$date2),7),
                #                        limits = c(flag()$date2,as.Date(input$date2)))+
                scale_x_date(#date_breaks = "1 week",
                    #breaks = seq(as.Date(input$date1),as.Date(input$date2),"7 days"),
                    breaks = seq(flag()$date2,as.Date(input$date2),"7 days"),
                    #date_labels = "%y-%m-%d",
                    limits = c(flag()$date2,input$date2)
                )+
                #scale_y_continuous(breaks = seq(0,100,20))+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
    
        
    output$line <- renderPlot({
        data4()%>%
            filter(Residential_City%in%input$pre)%>%
            ggplot2::ggplot(aes(x=Date,y=count_j,color=Residential_City))+
            ggplot2::geom_line()+
            labs(y="10万人当たりの感染者数",
                 color="市区町村")+
            # scale_x_continuous(breaks=seq(flag()$date2,as.Date(input$date2),7),
            #                        limits = c(flag()$date2,as.Date(input$date2)))+
            scale_x_date(#date_breaks = "1 week",
                #breaks = seq(as.Date(input$date1),as.Date(input$date2),"7 days"),
                breaks = seq(flag()$date2,as.Date(input$date2),"7 days"),
                #date_labels = "%y-%m-%d",
                         limits = c(flag()$date2,input$date2)
                )+
            scale_y_continuous(breaks = seq(0,100,20))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    data8<-
        reactive({
            #累積7日_1
            data5_1<-
                data5%>%
                filter(Fixed_Date<=input$date2,
                       Fixed_Date>=input$date2-6)%>%
                count(Residential_City,jinko)%>%
                rename("count_1"="n")%>%
                mutate(count1_j7=count_1/jinko*100000)%>%
                filter(!is.na(count1_j7))%>%
                select(Residential_City,count_1,count1_j7)
            #累積28日_1
            data5_2<-
                data5%>%
                filter(Fixed_Date<=input$date2,
                       Fixed_Date>=input$date2-27)%>%
                count(Residential_City,jinko)%>%
                rename("count28_1"="n")%>%
                mutate(count1_j28=count28_1/jinko*100000)%>%
                filter(!is.na(count1_j28))%>%
                select(Residential_City,count28_1,count1_j28)
            data6<-
                left_join(data5_1,data5_2)
            
            #累積7日_2
            data5_3<-
                data5%>%
                filter(Fixed_Date<=input$date2-7,
                       Fixed_Date>=input$date2-6-7)%>%
                count(Residential_City,jinko)%>%
                rename("count_2"="n")%>%
                mutate(count2_j7=count_2/jinko*100000)%>%
                filter(!is.na(count2_j7))%>%
                select(Residential_City,count_2,count2_j7)
            #累積28日_2
            data5_4<-
                data5%>%
                filter(Fixed_Date<=input$date2-7,
                       Fixed_Date>=input$date2-27-7)%>%
                count(Residential_City,jinko)%>%
                rename("count28_2"="n")%>%
                mutate(count2_j28=count28_2/jinko*100000)%>%
                filter(!is.na(count2_j28))%>%
                select(Residential_City,count28_2,count2_j28)
            data7<-
                left_join(data5_3,data5_4)

            inner_join(data6,data7)
        })
    output$line2<-
        renderPlot({
            
           
            data8()%>%
                #filter(Residential_City%in%input$pre)%>%
                ggplot(aes(x=count1_j7,y=count1_j28,colour=ifelse(Residential_City%in%input$pre,"選択した市区町村","それ以外の市区町村")))+
                geom_segment(aes(xend=count2_j7,yend=count2_j28))+
                geom_point()+
                geom_text_repel(aes(label=Residential_City))+
                #geom_text(aes(label=Residential_City))+
                labs(x="累積7日",y="累積28日",colour="市区町村")+
                geom_vline(xintercept=c(0,2.5,15,25),colour = c("black", "yellow","orange","red"))+
                geom_text(aes(x=7.5,y=10,label="ステージ2"))+
                geom_text(aes(x=20,y=10,label="ステージ3"))+
                geom_text(aes(x=45,y=10,label="ステージ4"))+
                scale_x_continuous(breaks = seq(0,input$num1,5),limits = c(0,input$num1))+
                scale_y_continuous(breaks = seq(0,input$num2,20),limits = c(0,input$num2))+
                ggtitle(paste0("人口10万人当たりの累積感染者数（",input$date2,"先週比）"))+
                scale_colour_manual(values = c("選択した市区町村"="red","それ以外の市区町村"="black"))
            
        })
    data9<-
        data%>%
        count(Residential_City,Fixed_Date)%>%
        filter(Residential_City%in%c("横浜市","川崎市川崎区","川崎市幸区",
                                     "川崎市中原区","川崎市高津区","川崎市多摩区",
                                     "川崎市宮前区", "川崎市麻生区","相模原市",
                                     "横須賀市","平塚市","鎌倉市","藤沢市",
                                     "小田原市","茅ヶ崎市","逗子市","三浦市",
                                     "秦野市","厚木市","大和市","伊勢原市",
                                     "海老名市","座間市","南足柄市",
                                     "綾瀬市","葉山町","寒川町","大磯町",
                                     "二宮町","中井町","大井町","松田町",
                                     "山北町","開成町","箱根町","真鶴町",
                                     "湯河原町","愛川町","清川村"))%>%
        tidyr::complete(Fixed_Date=tidyr::full_seq(Fixed_Date,1),Residential_City,fill = list(n = 0))%>%
        group_by(Residential_City)%>%
        mutate(zen=ifelse(n==0|lag(n)==0,0,n/lag(n)))%>%
        ungroup()
                
    data10<-reactive({
        te<-
            data9%>%
            filter(Fixed_Date<=input$date2,Fixed_Date>=input$date2-6)%>%
            #filter(Fixed_Date<="2021-06-01",Fixed_Date>="2021-05-25")%>%
            group_by(Residential_City)%>%
            mutate(sum=sum(zen))%>%
            filter(sum!=0)%>%
            summarise(kika1=geomean(zen,zneg.rm = T,na.rm=T))%>%
            left_join(data8(),by="Residential_City")
            
    })
    data11<-reactive({
        data9%>%
            filter(Fixed_Date<=input$date2-7,Fixed_Date>=input$date2-6-7)%>%
            group_by(Residential_City)%>%
            summarise(kika2=geomean(zen,zneg.rm = T,na.rm=T))%>%
            left_join(data10(),by="Residential_City")%>%
            filter(kika1!=0&kika2!=0)
        
    })
    output$line3<-
        renderPlot({
            data11()%>%
                ggplot(aes(x=count1_j7,y=kika1,colour=ifelse(Residential_City%in%input$pre,"選択した市区町村","それ以外の市区町村")))+
                geom_segment(aes(xend=count2_j7,yend=kika2))+
                geom_point()+
                geom_text_repel(aes(label=Residential_City))+
                geom_vline(xintercept=c(0,2.5,15,25),colour = c("black", "yellow","orange","red"))+
                geom_text(aes(x=7.5,y=1.4,label="ステージ2"))+
                geom_text(aes(x=20,y=1.4,label="ステージ3"))+
                geom_text(aes(x=45,y=1.4,label="ステージ4"))+
                geom_hline(yintercept=c(1.0,1.05,1.10,1.15),colour = c("black", "yellow","orange","red"))+
                geom_text(aes(x=35,y=1,label="同数"))+
                geom_text(aes(x=40,y=1.05,label="14日で倍"))+
                geom_text(aes(x=45,y=1.10,label="7日で倍"))+
                geom_text(aes(x=50,y=1.15,label="5日で倍"))+
                labs(x="累積7日",y="増加率",colour="市区町村")+
                scale_x_continuous(breaks = seq(0,input$num3,5),limits = c(0,input$num3))+
                scale_y_continuous(breaks = seq(0.8,input$num4,0.2),limits = c(0.8,input$num4))+
                ggtitle(paste0("人口10万人当たりの累積感染者数と増加率平均（",input$date2,"）"))+
                scale_colour_manual(values = c("選択した市区町村"="red","それ以外の市区町村"="black"))
            
        })
    output$line4<-
        renderPlot({
            data11()%>%
                ggplot(aes(x=kika1,y=kika2,colour=ifelse(Residential_City%in%input$pre,"選択した市区町村","それ以外の市区町村")))+
                geom_point()+
                #geom_smooth(method = "lm")+
                geom_text_repel(aes(label=Residential_City))+
                geom_vline(xintercept=c(1.0,1.05,1.10,1.15),colour = c("black", "yellow","orange","red"))+
                geom_hline(yintercept=c(1.0,1.05,1.10,1.15),colour = c("black", "yellow","orange","red"))+
                geom_text(aes(x=1,y=1.4,label="同数", angle=90))+
                geom_text(aes(x=1.05,y=1.4,label="14日で倍", angle=90))+
                geom_text(aes(x=1.10,y=1.4,label="7日で倍", angle=90))+
                geom_text(aes(x=1.15,y=1.4,label="5日で倍", angle=90))+
                geom_text(aes(x=1.4,y=1,label="同数"))+
                geom_text(aes(x=1.4,y=1.05,label="14日で倍"))+
                geom_text(aes(x=1.4,y=1.10,label="7日で倍"))+
                geom_text(aes(x=1.4,y=1.15,label="5日で倍"))+
                scale_x_continuous(breaks = seq(0.8,input$num5,0.2),limits = c(0.8,input$num5))+
                scale_y_continuous(breaks = seq(0.8,input$num6,0.2),limits = c(0.8,input$num6))+
                labs(x="増加率今週",y="増加率先週",colour="市区町村")+
                ggtitle(paste("増加率",input$date2,"対先週"))+
                scale_colour_manual(values = c("選択した市区町村"="red","それ以外の市区町村"="black"))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
