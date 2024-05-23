
EXECUTE dbms_output.put_line('');
EXECUTE dbms_output.put_line('KidneyStoneSVM node started: ' || SYSTIMESTAMP);
EXECUTE dbms_output.put_line('');

DECLARE
  v_caseid                  VARCHAR2(130);
  v_caseid_alias            VARCHAR2(130);
  v_target                  VARCHAR2(130);
  v_input_data              VARCHAR2(130);
  v_build_data              VARCHAR2(130);
  v_test_data               VARCHAR2(130);
  v_apply_data              VARCHAR2(130);
  v_index                   VARCHAR2(130);
  v_data_usage              VARCHAR2(130);
  v_weights_setting         VARCHAR2(130);
  v_build_setting           VARCHAR2(130);
  v_cost_setting            VARCHAR2(130);
  v_test_metric             VARCHAR2(130);
  v_confusion_matrix        VARCHAR2(130);
  v_confusion_matrix_part   VARCHAR2(130);
  v_lift                    VARCHAR2(130);
  v_lift_part               VARCHAR2(130);
  v_roc                     VARCHAR2(130);
  v_roc_part                VARCHAR2(130);
  v_lexer_name              VARCHAR2(130);
  v_auto_filter_name        VARCHAR2(130);
  v_stoplist_name           VARCHAR2(130);
  v_policy_name             VARCHAR2(130);
  v_row_diag_table          VARCHAR2(130);
  v_accuracy                NUMBER;
  v_accuracy_nested         DM_NESTED_NUMERICALS;
  v_avg_accuracy            NUMBER;
  v_predictive_conf         NUMBER;
  v_area_under_curve        NUMBER;
  v_area_under_curve_nested DM_NESTED_NUMERICALS;
  v_area_under_curve_result VARCHAR2(130);
  v_num_row_alias           VARCHAR2(130);
  TYPE ODMR_OBJECT_VALUES is TABLE OF VARCHAR2(4000);
  v_partitions              ODMR_OBJECT_VALUES;
  v_targets                 ODMR_OBJECT_VALUES;
  v_xlst                    dbms_data_mining_transform.TRANSFORM_LIST;
  v_sql                     CLOB;
  v_sql2                    CLOB;
  v_user_session            VARCHAR2(130) := SYS_CONTEXT ('USERENV', 'SESSION_USER');
  v_drop                    VARCHAR2(30) := '&DROP_EXISTING_OBJECTS';
  
  FUNCTION generateUniqueName RETURN VARCHAR2 IS
    v_uniqueName  VARCHAR2(130);
  BEGIN
    SELECT 'ODMR$'||TO_CHAR(SYSTIMESTAMP,'HH24_MI_SS_FF')||dbms_random.string(NULL, 7) INTO v_uniqueName FROM dual;
    RETURN v_uniqueName;
  END;
    
  FUNCTION getInputSource(p_nodeId VARCHAR2) RETURN VARCHAR2 IS
    v_output  VARCHAR2(261);
  BEGIN
    SELECT OUTPUT_NAME INTO v_output FROM "&WORKFLOW_OUTPUT" WHERE NODE_ID = p_nodeId AND COMMENTS = 'Output Data';
    RETURN v_output;
  END;

  FUNCTION getTextPolicy(p_nodeId VARCHAR2, p_column VARCHAR2 DEFAULT NULL) RETURN VARCHAR2 IS
    v_output  VARCHAR2(130);
  BEGIN
    IF (p_column IS NULL) THEN
      SELECT OUTPUT_NAME INTO v_output FROM "&WORKFLOW_OUTPUT" WHERE NODE_ID = p_nodeId AND OUTPUT_TYPE = 'POLICY' AND ADDITIONAL_INFO IS NULL;
    ELSE
      SELECT OUTPUT_NAME INTO v_output FROM "&WORKFLOW_OUTPUT" WHERE NODE_ID = p_nodeId AND OUTPUT_TYPE = 'POLICY' AND ADDITIONAL_INFO = 'Column='||p_column;
    END IF;
    RETURN v_output;
  END;

  PROCEDURE recordOutput(p_NODE_ID VARCHAR2, p_NODE_NAME VARCHAR2, p_NODE_TYPE VARCHAR2, 
                         p_MODEL_ID VARCHAR2, p_MODEL_NAME VARCHAR2, p_MODEL_TYPE VARCHAR2, 
                         p_OUTPUT_NAME VARCHAR2, p_OUTPUT_TYPE VARCHAR2, p_ADDITIONAL_INFO VARCHAR2, p_COMMENTS VARCHAR2) IS
  BEGIN
    INSERT INTO "&WORKFLOW_OUTPUT" VALUES (p_NODE_ID, p_NODE_NAME, p_NODE_TYPE, p_MODEL_ID, REPLACE(REPLACE(p_MODEL_NAME,'"',''), (v_user_session||'.'), ''), p_MODEL_TYPE, p_OUTPUT_NAME, p_OUTPUT_TYPE, p_ADDITIONAL_INFO, SYSTIMESTAMP, p_COMMENTS);
    COMMIT;
  END;

  PROCEDURE execSQL(p_sql CLOB) IS
    curid         INTEGER;
    ignoreid      INTEGER;    
  BEGIN
    curid := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE(curid, p_sql, DBMS_SQL.NATIVE);
    ignoreid := DBMS_SQL.EXECUTE(curid);
    DBMS_SQL.CLOSE_CURSOR(curid);
  EXCEPTION WHEN OTHERS THEN
    IF DBMS_SQL.IS_OPEN(curid) THEN
      DBMS_SQL.CLOSE_CURSOR(curid);
    END IF;
    RAISE;
  END;

  FUNCTION formatErrorStack(
    p_node_name IN VARCHAR2,
    p_sqlerr        IN VARCHAR2,
    p_error_stack   IN VARCHAR2 ) RETURN VARCHAR2
  IS
  BEGIN
    RETURN SUBSTR('Error in ' || p_node_name || ': ' || CHR(13) || CHR(10) || p_sqlerr || 
                   CHR(13) || CHR(10) || p_error_stack, 1, 4000);
  END;

BEGIN
  -- input view
  v_caseid := 'ID';
  v_target := 'TARGET';
  
  v_input_data := generateUniqueName;
   
  v_sql := 
    'CREATE VIEW '||v_input_data||' AS SELECT /*+ NO_PARALLEL */ * FROM 
    ( 
      SELECT "'||v_caseid||'", 
        "CALC",
        "COND",
        "GRAVITY",
        "OSMO",
        "PH",
        "TARGET",
        "UREA" 
      FROM '||getInputSource('10001')||' WHERE LENGTH(TRIM('||v_target||')) IS NOT NULL 
    )'; 
  execSQL(v_sql); 
   
  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', NULL, NULL, NULL, v_input_data, 'VIEW', NULL, 'Input Data'); 


  v_caseid_alias := generateUniqueName; 
  v_num_row_alias := generateUniqueName; 
  v_build_data := generateUniqueName; 
  v_test_data := generateUniqueName; 

   
  v_sql := 
    'CREATE TABLE '||v_build_data||' 
         NOPARALLEL NOLOGGING COMPRESS 
    AS SELECT /*+ NO_PARALLEL */ * FROM 
    ( 
      SELECT "'||v_caseid||'", 
        "CALC",
        "COND",
        "GRAVITY",
        "OSMO",
        "PH",
        "TARGET",
        "UREA" 
        FROM ( 
          WITH  
          "A" as (SELECT  * FROM '||v_input_data||'), 
          "B.1" as (SELECT  row_number() OVER(partition by "'||v_target||'" ORDER BY ORA_HASH("'||v_caseid||'")) '||v_caseid_alias||', "'||v_caseid||'" FROM "A" t), 
          "B.2" as (SELECT  t.*, p.'||v_caseid_alias||' FROM "A" t, "B.1" p WHERE t."'||v_caseid||'" = p."'||v_caseid||'"), 
          "B.3" as (SELECT "'||v_target||'", COUNT(*) '||v_num_row_alias||' FROM "A" GROUP BY "'||v_target||'"), 
          "B" as ( 
            SELECT  v1.* FROM "B.2" v1 ,"B.3" v2 
            WHERE v1."'||v_target||'" = v2."'||v_target||'" 
            AND ORA_HASH(v1.'||v_caseid_alias||', v2.'||v_num_row_alias||'-1, 0) <= (v2.'||v_num_row_alias||' * 60.0 / 100) 
          ) 
          SELECT * from "B" 
        ) 
    )'; 
  execSQL(v_sql); 
   

   
  v_sql := 
    'CREATE TABLE '||v_test_data||' 
         NOPARALLEL NOLOGGING COMPRESS 
    AS SELECT /*+ NO_PARALLEL */ * FROM 
    ( 
      SELECT "'||v_caseid||'", 
        "CALC",
        "COND",
        "GRAVITY",
        "OSMO",
        "PH",
        "TARGET",
        "UREA" 
        FROM ( 
          WITH 
          "A" as (SELECT  * FROM '||v_input_data||'), 
          "B.1" as (SELECT  row_number() OVER(partition by "'||v_target||'" ORDER BY ORA_HASH("'||v_caseid||'")) '||v_caseid_alias||', "'||v_caseid||'" FROM "A" t), 
          "B.2" as (SELECT  t.*, p.'||v_caseid_alias||' FROM "A" t, "B.1" p WHERE t."'||v_caseid||'" = p."'||v_caseid||'"),  
          "B.3" as (SELECT "'||v_target||'", COUNT(*) '||v_num_row_alias||' FROM "A" GROUP BY "'||v_target||'"),  
          "B" as ( 
            SELECT  v1.* FROM "B.2" v1 ,"B.3" v2 
            WHERE v1."'||v_target||'" = v2."'||v_target||'" 
            AND ORA_HASH(v1.'||v_caseid_alias||', v2.'||v_num_row_alias||'-1, 0) > (v2.'||v_num_row_alias||' * 60.0 / 100) 
          ) 
          SELECT * from "B" 
        ) 
    )'; 
  execSQL(v_sql); 
   
  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', NULL, NULL, NULL, v_build_data, 'TABLE', NULL, 'Build Data');
  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', NULL, NULL, NULL, v_test_data, 'TABLE', NULL, 'Test Data');


  -- data usage view
  v_data_usage := generateUniqueName;
  
  v_sql := 
    'CREATE VIEW '||v_data_usage||' AS SELECT /*+ NO_PARALLEL */ "'||v_caseid||'", 
        "CALC",
        "COND",
        "GRAVITY",
        "OSMO",
        "PH",
        "TARGET",
        "UREA" 
    FROM '||v_build_data;
  execSQL(v_sql);
  
  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_data_usage, 'VIEW', NULL, 'Data Usage');
  v_xlst := dbms_data_mining_transform.TRANSFORM_LIST(); 


  -- build setting
  v_build_setting := generateUniqueName;
  execSQL('CREATE TABLE '||v_build_setting||' (SETTING_NAME VARCHAR2(30), SETTING_VALUE VARCHAR2(130))'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES ('''||DBMS_DATA_MINING.SVMS_ACTIVE_LEARNING||''', ''SVMS_AL_ENABLE'')'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES ('''||DBMS_DATA_MINING.SVMS_COMPLEXITY_FACTOR||''', 0.1)'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES ('''||DBMS_DATA_MINING.SVMS_CONV_TOLERANCE||''', 0.001)'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES ('''||DBMS_DATA_MINING.SVMS_KERNEL_FUNCTION||''', ''SVMS_LINEAR'')'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES ('''||DBMS_DATA_MINING.ALGO_NAME||''', '''||DBMS_DATA_MINING.ALGO_SUPPORT_VECTOR_MACHINES||''')'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES ('''||DBMS_DATA_MINING.CLAS_WEIGHTS_BALANCED||''', ''ON'')'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES ('''||DBMS_DATA_MINING.PREP_AUTO||''', '''||DBMS_DATA_MINING.PREP_AUTO_ON||''')'); 
  execSQL('INSERT INTO '||v_build_setting||' VALUES (''ODMS_SAMPLING'', ''ODMS_SAMPLING_DISABLE'')'); 

  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_build_setting, 'TABLE', NULL, 'Build Setting');

  -- model build
  IF (v_drop = 'TRUE') THEN -- delete any existing model?
    BEGIN
      DBMS_DATA_MINING.DROP_MODEL('&MODEL_1', TRUE);
    EXCEPTION WHEN OTHERS THEN
      NULL; -- ignore if no existing model to drop
    END;
  END IF;
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => '&MODEL_1',
    mining_function     => DBMS_DATA_MINING.CLASSIFICATION,
    data_table_name     => v_data_usage,
    case_id_column_name => '"'||v_caseid||'"',
    target_column_name  => '"'||v_target||'"',
    settings_table_name => v_build_setting,
    xform_list          => v_xlst);
  execSQL('COMMENT ON MINING MODEL &MODEL_1 IS ''BALANCED'''); 

  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', '&MODEL_1', 'MODEL', NULL, 'Model');



  -- apply result for test
  v_apply_data := generateUniqueName;
  execSQL('CREATE TABLE '||v_apply_data||' NOPARALLEL NOLOGGING AS SELECT /*+ NO_PARALLEL */ "'||v_caseid||'", t.* FROM '||v_test_data||' s, TABLE(PREDICTION_SET(&MODEL_1 USING *)) t');
  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_apply_data, 'TABLE', NULL, 'Apply Data');

  -- test metric
  v_test_metric := generateUniqueName;
  execSQL('CREATE TABLE '||v_test_metric||' (METRIC_NAME VARCHAR2(30), METRIC_VARCHAR_VALUE VARCHAR2(128), METRIC_NUM_VALUE NUMBER)  ');
  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_test_metric, 'TABLE', NULL, 'Test Metric');

  execSQL('INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_VARCHAR_VALUE) VALUES (''MODEL_SCHEMA'', SUBSTR(''&MODEL_1'', 1 ,INSTR(''&MODEL_1'', ''.'', 1, 1)-1))');
  execSQL('INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_VARCHAR_VALUE) VALUES (''MODEL_NAME'', SUBSTR(''&MODEL_1'', INSTR(''&MODEL_1'',''.'', -1, 1)+1))');
  execSQL('INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_VARCHAR_VALUE) VALUES (''MINING_FUNCTION'', ''CLASSIFICATION'')');
  execSQL('INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_VARCHAR_VALUE) VALUES (''TARGET_ATTRIBUTE'', '''||v_target||''')');

  EXECUTE IMMEDIATE 'SELECT "'||v_target||'" FROM '||v_test_data||' GROUP BY "'||v_target||'" ORDER BY count(*) ASC' BULK COLLECT INTO v_targets;
  execSQL('INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_VARCHAR_VALUE) VALUES (''LEAST_TARGET_VALUE'', '''||v_targets(1)||''')');

  execSQL('INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_NUM_VALUE) SELECT ''TEST_ROWS'', count(*) FROM '||v_test_data);

  -- confusion matrix
  v_confusion_matrix := generateUniqueName;
  DBMS_DATA_MINING.COMPUTE_CONFUSION_MATRIX (
    accuracy                    => v_accuracy,
    apply_result_table_name     => v_apply_data,
    target_table_name           => v_test_data,
    case_id_column_name         => '"'||v_caseid||'"',
    target_column_name          => '"'||v_target||'"',
    confusion_matrix_table_name => v_confusion_matrix,
    score_column_name           => 'PREDICTION',
    score_criterion_column_name => 'PROBABILITY',
    score_criterion_type        => 'PROBABILITY');
  execSQL('INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_NUM_VALUE) VALUES (''ACCURACY'', NVL('||v_accuracy||', 0)*100)');
  recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_confusion_matrix, 'TABLE', NULL, 'Confusion Matrix');
  
  -- average accuracy
  v_sql := 
    'INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_NUM_VALUE)
    WITH
    a as
      (SELECT a.actual_target_value, sum(a.value) recall_total
         FROM '||v_confusion_matrix||' a
         group by a.actual_target_value)
      ,
    b as
      (SELECT count(distinct b.actual_target_value) num_recalls
         FROM '||v_confusion_matrix||' b)
      ,
    c as
      (SELECT c.actual_target_value, value
         FROM '||v_confusion_matrix||' c
         where actual_target_value = predicted_target_value)
      ,
    d as
      (SELECT NVL(sum(c.value/GREATEST(0.0001, a.recall_total)), 0) tot_accuracy
         FROM a, c
         where a.actual_target_value = c.actual_target_value(+))
    SELECT ''AVG_ACCURACY'', d.tot_accuracy/GREATEST(0.0001, b.num_recalls) * 100 avg_accuracy
    FROM b, d';
  execSQL(v_sql);

  -- predictive confidence
  v_sql := 
    'INSERT INTO '||v_test_metric||' (METRIC_NAME, METRIC_NUM_VALUE)
    WITH
    a as
      (SELECT a.actual_target_value, sum(a.value) recall_total
         FROM '||v_confusion_matrix||' a
         group by a.actual_target_value)
      ,
    b as
      (SELECT count(distinct b.actual_target_value) num_classes
         FROM '||v_confusion_matrix||' b)
      ,
    c as
      (SELECT c.actual_target_value, value
         FROM '||v_confusion_matrix||' c
         WHERE actual_target_value = predicted_target_value)
      ,
    d as
      (SELECT NVL(sum(c.value/a.recall_total), 0) tot_accuracy
         FROM a, c
         WHERE a.actual_target_value = c.actual_target_value(+))
    SELECT ''PREDICTIVE_CONFIDENCE'', GREATEST(0, ((1 - (1 - d.tot_accuracy/GREATEST(0.0001, b.num_classes)) / GREATEST(0.0001, ((b.num_classes-1)/GREATEST(0.0001, b.num_classes)))) * 100))
    FROM b, d';
  execSQL(v_sql);
  

  -- targets for test results 
  v_sql := 
    'SELECT /*+ NO_PARALLEL */ "'||v_target||'" as prediction FROM 
    ( 
      SELECT "'||v_target||'", 
      RANK() OVER (ORDER BY count("'||v_target||'") DESC) "Rank" 
      FROM '||v_test_data||'  
      GROUP BY "'||v_target||'" 
    ) 
    WHERE rownum <= 5'; 
  EXECUTE IMMEDIATE v_sql BULK COLLECT INTO v_targets; 

  FOR i IN 1..v_targets.COUNT LOOP 
    -- lift for each target 
    v_lift := generateUniqueName; 
    DBMS_DATA_MINING.COMPUTE_LIFT ( 
      apply_result_table_name   => v_apply_data, 
      target_table_name         => v_test_data, 
      case_id_column_name       => v_caseid, 
      target_column_name        => '"'||v_target||'"', 
      lift_table_name           => v_lift, 
      positive_target_value     => v_targets(i), 
      score_column_name         => 'PREDICTION', 
      score_criterion_column_name => 'PROBABILITY', 
      num_quantiles             => 100, 
      score_criterion_type      => 'PROBABILITY'); 
    recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_lift, 'TABLE', v_target||'='||v_targets(i), 'Lift Result'); 
  END LOOP; 

  -- roc for each target (only binary target support) 
  IF (v_targets.COUNT <= 2) THEN 
    FOR i IN 1..v_targets.COUNT LOOP 
      v_roc := generateUniqueName; 
      DBMS_DATA_MINING.COMPUTE_ROC ( 
        roc_area_under_curve        => v_area_under_curve, 
        apply_result_table_name     => v_apply_data, 
        target_table_name           => v_test_data, 
        case_id_column_name         => v_caseid, 
        target_column_name          => '"'||v_target||'"', 
        roc_table_name              => v_roc, 
        positive_target_value       => v_targets(i), 
        score_column_name           => 'PREDICTION', 
        score_criterion_column_name => 'PROBABILITY'); 
      recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_roc, 'TABLE', v_target||'='||v_targets(i), 'ROC Result'); 
      recordOutput('10037', 'KidneyStoneSVM', 'ClassificationBuildNode', '10035', '&MODEL_1', 'Support Vector Machine', v_area_under_curve, 'SCALAR', v_target||'='||v_targets(i), 'ROC Area Under Curve'); 
    END LOOP; 
  END IF; 



EXCEPTION WHEN OTHERS THEN
  RAISE_APPLICATION_ERROR(-20999, formatErrorStack('KidneyStoneSVM', SQLERRM, DBMS_UTILITY.FORMAT_ERROR_BACKTRACE()));
END;
/
EXECUTE dbms_output.put_line('');
EXECUTE dbms_output.put_line('KidneyStoneSVM node completed: ' || SYSTIMESTAMP);
EXECUTE dbms_output.put_line('');
