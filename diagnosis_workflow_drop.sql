
WHENEVER OSERROR EXIT;
WHENEVER SQLERROR EXIT SQL.SQLCODE;

EXECUTE dbms_output.put_line('');
EXECUTE dbms_output.put_line('Cleanup started: ' || SYSTIMESTAMP);
EXECUTE dbms_output.put_line('');

DECLARE
 v_version VARCHAR(1) ;
BEGIN
  SELECT (CASE WHEN VERSION >= '11.2.0.3' THEN 1 ELSE 0 END) AS V_11_UP 
  INTO v_version FROM PRODUCT_COMPONENT_VERSION
  WHERE PRODUCT like 'Oracle Database 1%' OR PRODUCT like 'Personal Oracle Database 1%';
  
  IF(v_version = '0') THEN
    EXECUTE IMMEDIATE 'ALTER SESSION set "_optimizer_reuse_cost_annotations"=false';
  END IF;
  EXECUTE IMMEDIATE 'ALTER SESSION set NLS_NUMERIC_CHARACTERS=".,"';
END;
/

-- SQL Plus Script Commands Section:
SET SERVEROUTPUT ON
SET VERIFY OFF

-- Workflow output table
DEFINE WORKFLOW_OUTPUT = 'DIAGNOSIS_WORKFLOW'

-- Clean up objects
DECLARE
  TYPE OBJ_ARRAY IS TABLE OF VARCHAR2(261);
  v_objs OBJ_ARRAY;
  v_types OBJ_ARRAY;
  table_cnt NUMBER;
BEGIN
  SELECT count(*) INTO table_cnt FROM user_tables WHERE table_name='&WORKFLOW_OUTPUT';
  IF (table_cnt > 0) THEN
    EXECUTE IMMEDIATE 'SELECT OUTPUT_NAME, OUTPUT_TYPE FROM "&WORKFLOW_OUTPUT"' BULK COLLECT INTO v_objs, v_types;
    FOR i in 1..v_objs.COUNT LOOP
      BEGIN
        IF (v_types(i) = 'TABLE') THEN
          EXECUTE IMMEDIATE 'DROP TABLE '||v_objs(i)||' PURGE';
          DBMS_OUTPUT.PUT_LINE('Drop '||v_types(i)||': '||v_objs(i));
        ELSIF (v_types(i) = 'VIEW') THEN
          EXECUTE IMMEDIATE 'DROP VIEW '||v_objs(i);
          DBMS_OUTPUT.PUT_LINE('Drop '||v_types(i)||': '||v_objs(i));
        ELSIF (v_types(i) = 'MODEL') THEN
          DBMS_DATA_MINING.DROP_MODEL(v_objs(i), TRUE);
          DBMS_OUTPUT.PUT_LINE('Drop '||v_types(i)||': '||v_objs(i));
        ELSIF (v_types(i) = 'STOPLIST') THEN
          ctx_ddl.DROP_STOPLIST(stoplist_name => v_objs(i));
          DBMS_OUTPUT.PUT_LINE('Drop '||v_types(i)||': '||v_objs(i));
        ELSIF (v_types(i) = 'LEXER') THEN
          ctx_ddl.DROP_PREFERENCE(preference_name => v_objs(i));
          DBMS_OUTPUT.PUT_LINE('Drop '||v_types(i)||': '||v_objs(i));
        ELSIF (v_types(i) = 'AUTOFILTER') THEN
          ctx_ddl.DROP_PREFERENCE(preference_name => v_objs(i));
          DBMS_OUTPUT.PUT_LINE('Drop '||v_types(i)||': '||v_objs(i));
        ELSIF (v_types(i) = 'POLICY') THEN
          ctx_ddl.DROP_POLICY(policy_name => v_objs(i));
          DBMS_OUTPUT.PUT_LINE('Drop '||v_types(i)||': '||v_objs(i));
        END IF;
      EXCEPTION WHEN OTHERS THEN
        dbms_output.put_line('Failed to drop '||v_types(i)||': '||v_objs(i));
      END;
    END LOOP;
    BEGIN
      EXECUTE IMMEDIATE 'DROP TABLE "&WORKFLOW_OUTPUT" PURGE';
      dbms_output.put_line('Workflow output table dropped: &WORKFLOW_OUTPUT');
    EXCEPTION WHEN OTHERS THEN
      dbms_output.put_line('Failed to drop Workflow output table: &WORKFLOW_OUTPUT');
    END;
  ELSE
    dbms_output.put_line('Workflow output table does not exist: &WORKFLOW_OUTPUT');
  END IF;
END;
/
EXECUTE dbms_output.put_line('');
EXECUTE dbms_output.put_line('Cleanup completed: ' || SYSTIMESTAMP);
EXECUTE dbms_output.put_line('');
