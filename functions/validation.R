#QC Config validation rules

# observeEvent(
#   input$lowlevel1 | input$highlevel1,
#   {
#     low1iv = InputValidator$new()
#     if (!is.null(input$highlevel1)){
#       
#       hl1 = input$highlevel1
#       low1iv$add_rule("lowlevel1",sv_between(0,hl1))
#       low1iv$enable()
#     }
#   })
# 
# observeEvent(
#   input$lowlevel1 | input$highlevel1 | input$lowlevel2,
#   {
#     high1iv = InputValidator$new()
#     if (!is.null(input$lowlevel1) & !is.null(input$lowlevel2)){
#       ll1 = input$lowlevel1
#       ll2 = input$lowlevel2
#       high1iv$add_rule("highlevel1",sv_between(ll1,ll2))
#     }else if (!is.null(input$lowlevel1) & is.null(input$lowlevel2)){
#       ll1 = input$lowlevel1
#       high1iv$add_rule("highlevel1",sv_gt(ll1))
#     }else if (is.null(input$lowlevel1) & !is.null(input$lowlevels)){
#       ll2 = input$lowlevel2
#       high1iv$add_rule("highelevel1",sv_lt(ll2))
#       
#     }
#     high1iv$enable()
# 
# 
# 
# 
#   }
# )