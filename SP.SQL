
USE [RosterReporting]
GO

/****** Object:  StoredProcedure [dbo].[GetGroupDefaultPredictions]    Script Date: 11/30/2017 5:23:18 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



ALTER PROCEDURE [dbo].[GetGroupDefaultPredictions] @DistrictIDList VARCHAR(max) = NULL
	,@CountryID INT = NULL
AS
BEGIN
	DECLARE @InputData1 NVARCHAR(max)

	IF (@DistrictIDList is NULL and @CountryID is NULL)
	BEGIN

	RAISERROR ('Both @DistrictIDList and @CountryID parameters cannot be NULL at the same time.', -- Message text.
               16, -- Severity.
               1 -- State.
               );
	RETURN
	END


	IF @CountryID IS NULL
	BEGIN
		SET @InputData1 = 'Select * from [RosterReporting].[dbo].SCVR where DistrictID in (' + @DistrictIDList + ')'
	END
	ELSE
	BEGIN
		SET @InputData1 = 'Select * from [RosterReporting].[dbo].SCVR where DistrictID%1000 =' + CAST(@CountryID AS VARCHAR)
	END

	
	DECLARE @Results TABLE (
		GroupUniq VARCHAR(max) NOT NULL
		,DistrictID INT NOT NULL
		,SiteID INT
		,GroupID INT
		,PercentToDate DECIMAL(18, 3)
		,DefaultProbability DECIMAL(18, 3)
		,WillDefault BIT
		)

	INSERT INTO @Results
	EXEC sp_execute_external_script @language = N'R'
		,@script = N'source("C:/R_default/Data_cleaning.R");
		datout <- process_data(InputDataSet, fakedate="2017-06-06");
		d2018 <- datout[[length(datout)]];
		print(head(d2018,2));
		datout[[as.integer(format(Sys.Date(), "%Y")) + 1]] <- NULL;
		source("C:/R_default/train_model.R");
		RF_objs <- train_RF_on_data("./", datout, full_mode = FALSE, 4);
		source("C:/R_default/predict_default.R");
		predsouty <- RF_predictions(d2018, RF_objs[[3]], RF_objs, "./");
		print(head(predsouty));	'
		,@input_data_1 = @InputData1
		,@input_data_1_name = N'InputDataSet'
		,@output_data_1_name = N'predsouty'
		,@parallel = 1

	/*WITH RESULT SETS((
					GroupUniq VARCHAR(max) NOT NULL
					,DistrictID INT NOT NULL
					,SiteID INT
					,GroupID INT
					,PercentToDate DECIMAL(18, 3)
					,DefaultProbability DECIMAL(18, 3)
					,WillDefault BIT
					));*/
	SELECT d.DistrictName
		,s.SiteName
		,g.GroupName
		,r.PercentToDate
		,r.DefaultProbability
		,r.WillDefault
	FROM @Results AS r
	INNER JOIN Groups AS g ON r.DistrictID = g.DistrictID
		AND r.SiteiD = g.SiteID
		AND r.GroupID = g.GroupID
	INNER JOIN Sites AS s ON r.DistrictID = s.DistrictID
		AND r.SiteID = s.SiteID
	INNER JOIN Districts AS d ON r.DistrictID = d.DistrictID
	ORDER BY r.DistrictID
		,r.SiteID
		,r.GroupID
END
GO