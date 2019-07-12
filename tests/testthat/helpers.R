# A helper function for testing: passes if the user looks at the plot and
# presses enter, fails if the user enters anything else

# expect_manual_OK <- function(test.id, prompt="Look at the plot.") {
#   #user.input <- readline(paste0("Test '",test.id, "': ", prompt, " Hit ENTER for OK; enter any character otherwise: "))
#   user.gives.OK <- TRUE
#   expect_true(user.gives.OK, label=paste0("Test '",test.id,"' okayed by user"))
# }

# A test for expect_manual_OK:
# test_that("expect_manual_OK makes sense as a helper function", {
#   plot(1:25, pch=1:25)
#   expect_manual_OK(1)
#
#   print(data.frame(x=1:3,y=6:8))
#   expect_manual_OK("'data.frame looks good'", "Look at the table. ")
# })
