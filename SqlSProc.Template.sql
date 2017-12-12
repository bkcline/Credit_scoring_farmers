CREATE PROCEDURE [SqlSProc]
AS
BEGIN

EXEC sp_execute_external_script 
	@language = N'R'
    , @script = N'source("C:/R_default/Data_cleaning.R");
	datout <- process_data(InputDataSet, fakedate="2017-06-06");
	d2018 <- datout[[length(datout)]];
	print(head(d2018,2));
	datout[[as.integer(format(Sys.Date(), "%Y")) + 1]] <- NULL;
	source("C:/R_default/train_model.R");
	RF_objs <- train_RF_on_data("./", datout, full_mode = FALSE, 4);
	source("C:/R_default/predict_default.R");
	predsouty <- RF_predictions(d2018, RF_objs[[3]], RF_objs, "./");
	print(head(predsouty));	'
    , @input_data_1 = N'Select * from [RosterReporting].[dbo].SCVR where DistrictID%1000 = 404'
	,@input_data_1_name = N'InputDataSet' 
	, @parallel = 1
--- Edit this line to handle the output data frame.
  ---  WITH RESULT SETS (([MYNEWCOLUMN] NVARCHAR(max)));
  WITH RESULT SETS UNDEFINED;

END;


