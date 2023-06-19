



#' Title 预览工资数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples viewserver()
viewsalarystdserver <- function(input, output, session, dms_token) {
  shiny::observe({
    shiny::observeEvent(input$btn_view_salarystd,
                        {
                          # 工资std表异常检查
                          data = mdljhhrvPreCheckPkg::ds_salaryStdCheck_query(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                          
                          data = as.data.frame(data)
                          
                          data = tsdo::na_standard(data)
                          
                          #显示数据
                          tsui::run_dataTable2(id = 'view_data_std', data = data)
                          
                        })
    
  })
}


#' Title 预览社保数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples viewserver()
viewsocialsecuritystdserver <-
  function(input, output, session, dms_token) {
    shiny::observe({
      shiny::observeEvent(input$btn_view_socialsecuritystd,
                          {
                            # 社保std表异常检查
                            data = mdljhhrvPreCheckPkg::ds_socialSecuryStdCheck_query(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                            
                            data = as.data.frame(data)
                            
                            data = tsdo::na_standard(data)
                            
                            #显示数据
                            tsui::run_dataTable2(id = 'view_data_std', data = data)
                            
                            
                            
                          })
      
    })
  }



#' Title 更新异常工资数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples viewserver()
updatesalarystdserver <-
  function(input, output, session, dms_token) {
    shiny::observe({
      shiny::observeEvent(input$btn_update_salarystd,
                          {
                            # 工资std表异常处理
                            mdljhhrvPreCheckPkg::ds_salaryStdCheck_deal(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                            
                            tsui::pop_notice('数据更新成功')
                            
                            
                          })
      
    })
  }


#' Title 更新异常社保数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples viewserver()
updatesocialsecuritystdserver <-
  function(input, output, session, dms_token) {
    shiny::observe({
      shiny::observeEvent(input$btn_update_socialsecuritystd,
                          {
                            # 社保std表异常处理
                            mdljhhrvPreCheckPkg::ds_socialSecuryOdsCheck_deal(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                            
                            tsui::pop_notice('数据更新成功')
                            
                            
                          })
      
    })
  }




#' Title 后台处理总函数
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples HrvServer()
jhhrvstdprecheckServer <- function(input, output, session, dms_token) {
  #预览工资
  viewsalarystdserver(input, output, session, dms_token)
  #预览社保
  viewsocialsecuritystdserver(input, output, session, dms_token)
  
  #更新工资
  updatesalarystdserver(input, output, session, dms_token)
  # 更新社保
  updatesocialsecuritystdserver(input, output, session, dms_token)
  
}