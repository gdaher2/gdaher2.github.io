/*
Name of Program:	analysis.sas
Created by:			George Daher
Creation date:		11/27/2024
Inputs:				Data file: houseFinData.csv - a cleaned/formatted version with house campaign election result and financial 
									information for 2014, 2016, and 2018 
Outputs:			Summary Statistics, Frequency Tables, Bivariate Plots, Correlation table
 */

/*
Imports the Datasets from the given files in the course folder
*/
PROC IMPORT FILE="/home/u63993149/STAT 277/Project/houseFinData.csv"
	DBMS=CSV OUT=hfd REPLACE;
RUN;

PROC IMPORT FILE="/home/u63993149/STAT 277/Project/houseFinDataNoProp.csv"
	DBMS=CSV OUT=hfdNoProp REPLACE;
RUN;

PROC FORMAT;
	VALUE winInd 	0 = "0: Election Lost"
					1 = "1: Election Won";
RUN;

/* Creates a new dataset where all third parties (not dem or rep) are collapsed into one category */
DATA hfd2;
	SET hfdNoProp;
	FORMAT GE_WINNER_INDICATOR winInd.;
	IF NOT (PARTY = "D" OR PARTY = "R") THEN PARTY = "T";
RUN;

/* Creates a new dataset where all financial variables are log transformed, to normalize them */
DATA hfdLog;
	SET hfd2;
	Log_Total_receipts	= LOG(Total_receipts + 1);
	Log_Transfers_from_authorized_c	= LOG(Transfers_from_authorized_commi + 1);
	Log_Total_disbursements	= LOG(Total_disbursements + 1);
	Log_Transfers_to_authorized_com	= LOG(Transfers_to_authorized_committ + 1);
	Log_Beginning_cash	= LOG(Beginning_cash + 1);
	Log_Ending_cash	= LOG(Ending_cash + 1);
	Log_Contributions_from_candi = LOG(Contributions_from_candidate + 1);
	Log_Loans_from_candidate = LOG(Loans_from_candidate + 1);
	Log_Other_loans	= LOG(Other_loans + 1);
	Log_Candidate_loan_repaym = LOG(Candidate_loan_repayments + 1);
	Log_Other_loan_repaym = LOG(Other_loan_repayments + 1);
	Log_Debts_owed_by	= LOG(Debts_owed_by);
	Log_Total_individual_contribut	= LOG(Total_individual_contributions + 1);
	Log_Contributions_from_other_po	= LOG(Contributions_from_other_politi + 1);
	Log_Contributions_from_party_co	= LOG(Contributions_from_party_commit + 1);
	Log_Refunds_to_individuals	= LOG(Refunds_to_individuals + 1);
	Log_Refunds_to_committees = LOG(Refunds_to_committees + 1);
RUN;

ODS EXCLUDE ASSOCIATION;
PROC LOGISTIC data=hfdLog;
	MODEL GE_WINNER_INDICATOR(EVENT = "1: Election Won") = Log_Total_receipts Log_Transfers_from_authorized_c Log_Total_disbursements Log_Transfers_to_authorized_com Log_Beginning_cash Log_Ending_cash Log_Contributions_from_candi Log_Loans_from_candidate Log_Other_loans Log_Candidate_loan_repaym Log_Other_loan_repaym Log_Debts_owed_by Log_Total_individual_contribut Log_Contributions_from_other_po Log_Contributions_from_party_co Log_Refunds_to_individuals Log_Refunds_to_committees Total_receipts	Transfers_from_authorized_commi	Total_disbursements	Transfers_to_authorized_committ	Beginning_cash	Ending_cash	Contributions_from_candidate	Loans_from_candidate	Other_loans	Candidate_loan_repayments	Other_loan_repayments	Debts_owed_by	Total_individual_contributions	Contributions_from_other_politi	Contributions_from_party_commit	Refunds_to_individuals	Refunds_to_committees
               /  selection=stepwise
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit;
	output out=pred p=phat lower=lcl upper=ucl
          predprob=(individual crossvalidate);
	ods output Association=Association;
RUN;