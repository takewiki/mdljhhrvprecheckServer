


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
viewsalaryodsserver <- function(input, output, session, dms_token) {
  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_precheck_view_salaryods,
                        {
                          # 工资ODS表异常检查
                          data = mdljhhrvPreCheckPkg::ds_salaryOdsCheck_query(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                          
                          data = as.data.frame(data)
                          
                          data = tsdo::na_standard(data)
                          names(data) = c('检查项',
                                          '数据源'
                          )
                          
                          #显示数据
                          tsui::run_dataTable2(id = 'hrv_precheck_view_data_ods', data = data)
                          
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
viewsocialsecurityodsserver <- function(input, output, session, dms_token) {
  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_precheck_view_socialsecurityods,
                        {
                          # 社保ODS表异常检查
                          data = mdljhhrvPreCheckPkg::ds_socialSecuryOdsCheck_query(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                          
                          data = as.data.frame(data)
                          
                          data = tsdo::na_standard(data)
                          names(data) = c(
                                          '检查项',
                                          '数据源'
                          )
                          #显示数据
                          tsui::run_dataTable2(id = 'hrv_precheck_view_data_ods', data = data)
                          
                          
                          
                        })
    
  })
}


#' Title 预览规则数据
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
viewrulevoucherodsserver <- function(input, output, session, dms_token) {
  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_precheck_view_rulevoucherods,
                        {
                          # 凭证规则表异常检验主要是银行类型
                          data = mdljhhrvPreCheckPkg::rule_voucherCheck_query(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                          
                          data = as.data.frame(data)
                          
                          data = tsdo::na_standard(data)
                          names(data) = c(
                                          '检查项',
                                          '数据源'
                          )
                          #显示数据
                          tsui::run_dataTable2(id = 'hrv_precheck_view_data_ods', data = data)

                          
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
updatesalaryodsserver <- function(input, output, session, dms_token) {
  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_precheck_update_salaryods,
                        {
                          # 工资ODS表异常处理
                          mdljhhrvPreCheckPkg::ds_salaryOdsCheck_deal(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                          
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
updatesocialsecurityodsserver <- function(input, output, session, dms_token) {
  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_precheck_update_socialsecurityods,
                        {
                          # 社保ODS表异常处理
                          mdljhhrvPreCheckPkg::ds_socialSecuryOdsCheck_deal(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                          
                          tsui::pop_notice('数据更新成功')
                          
                          
                        })
    
  })
}


#' Title 更新异常规则数据
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
updaterulevoucherodsserver <- function(input, output, session, dms_token) {
  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_precheck_update_rulevoucherods,
                        {
                          # 凭证规则表异常处理
                          mdljhhrvPreCheckPkg::rule_voucherCheck_deal(FToken = "057A7F0E-F187-4975-8873-AF71666429AB")
                          
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
jhhrvodsprecheckServer <- function(input, output, session, dms_token) {
  #预览工资
  viewsalaryodsserver(input, output, session, dms_token)
  #预览社保
  viewsocialsecurityodsserver(input, output, session, dms_token)
  #预览规则
  viewrulevoucherodsserver(input, output, session, dms_token)
  #更新工资
  updatesalaryodsserver(input, output, session, dms_token)
  # 更新社保
  updatesocialsecurityodsserver(input, output, session, dms_token)
  # 更新规则
  updaterulevoucherodsserver(input, output, session, dms_token)
}