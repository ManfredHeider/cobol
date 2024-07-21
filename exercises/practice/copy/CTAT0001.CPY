      ******************************************************************
      * DCLGEN TABLE(T13PAS.TBAT0001)                                  *
      *        LIBRARY(P13SP14.USERTEST.SRCDCL(CTAT0001))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0001 TABLE
CCC004*    ( DOK_QUELLE                     VARCHAR(44) NOT NULL,
CCC004*      DOK_TS                         TIMESTAMP NOT NULL,
CCC004*      DOK_XML                        XML
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13PAS.TBAT0001                    *
      ******************************************************************
       01  DCLTBAT0001.
           10 DOK-QUELLE.
              49 DOK-QUELLE-LEN    PIC S9(4) USAGE COMP.
              49 DOK-QUELLE-TEXT   PIC X(44).
           10 DOK-TS               PIC X(26).
           10 DOK-XML              USAGE SQL TYPE IS XML AS CLOB(1M).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
      *       HIDDEN COLUMN
      *    10 DB2_GENERATED_DOCID_FOR_XML PIC S9(18) USAGE COMP.