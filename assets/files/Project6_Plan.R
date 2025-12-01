setwd("~/College/STAT 277/Project")
webl14 <- read.csv("~/College/STAT 277/Project/DATA/webl14.txt", header=FALSE,
                     sep='|')
colnames(webl14) <- c("CAND_ID","CAND_NAME","CAND_ICI","PTY_CD",
                        "CAND_PTY_AFFILIATION","TTL_RECEIPTS","TRANS_FROM_AUTH",
                        "TTL_DISB","TRANS_TO_AUTH","COH_BOP","COH_COP",
                        "CAND_CONTRIB","CAND_LOANS","OTHER_LOANS","CAND_LOAN_REPAY",
                        "OTHER_LOAN_REPAY","DEBTS_OWED_BY","TTL_INDIV_CONTRIB",
                        "CAND_OFFICE_ST","CAND_OFFICE_DISTRICT",
                        "SPEC_ELECTION","PRIM_ELECTION","RUN_ELECTION","GEN_ELECTION",
                        "GEN_ELECTION_PRESENT",
                        "OTHER_POL_CMTE_CONTRIB","POL_PTY_CONTRIB",
                        "CVG_END_DT","INDIV_REFUNDS","CMTE_REFUNDS")

webl16 <- read.csv("~/College/STAT 277/Project/DATA/webl16.txt", header=FALSE,
                   sep='|')
colnames(webl16) <- c("CAND_ID","CAND_NAME","CAND_ICI","PTY_CD",
                      "CAND_PTY_AFFILIATION","TTL_RECEIPTS","TRANS_FROM_AUTH",
                      "TTL_DISB","TRANS_TO_AUTH","COH_BOP","COH_COP",
                      "CAND_CONTRIB","CAND_LOANS","OTHER_LOANS","CAND_LOAN_REPAY",
                      "OTHER_LOAN_REPAY","DEBTS_OWED_BY","TTL_INDIV_CONTRIB",
                      "CAND_OFFICE_ST","CAND_OFFICE_DISTRICT",
                      "SPEC_ELECTION","PRIM_ELECTION","RUN_ELECTION","GEN_ELECTION",
                      "GEN_ELECTION_PRESENT",
                      "OTHER_POL_CMTE_CONTRIB","POL_PTY_CONTRIB",
                      "CVG_END_DT","INDIV_REFUNDS","CMTE_REFUNDS")

webl18 <- read.csv("~/College/STAT 277/Project/DATA/webl16.txt", header=FALSE,
                   sep='|')
colnames(webl18) <- c("CAND_ID","CAND_NAME","CAND_ICI","PTY_CD",
                      "CAND_PTY_AFFILIATION","TTL_RECEIPTS","TRANS_FROM_AUTH",
                      "TTL_DISB","TRANS_TO_AUTH","COH_BOP","COH_COP",
                      "CAND_CONTRIB","CAND_LOANS","OTHER_LOANS","CAND_LOAN_REPAY",
                      "OTHER_LOAN_REPAY","DEBTS_OWED_BY","TTL_INDIV_CONTRIB",
                      "CAND_OFFICE_ST","CAND_OFFICE_DISTRICT",
                      "SPEC_ELECTION","PRIM_ELECTION","RUN_ELECTION","GEN_ELECTION",
                      "GEN_ELECTION_PRESENT",
                      "OTHER_POL_CMTE_CONTRIB","POL_PTY_CONTRIB",
                      "CVG_END_DT","INDIV_REFUNDS","CMTE_REFUNDS")