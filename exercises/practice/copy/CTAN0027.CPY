      ****************************************************************
      *    COPY-MEMBER: CTAN0027                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0027 INHA             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01354!13.02.18!2.1  !NEU                        !RAHNER   *
      *         !        !     !                           !         *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME    *
      * ------------------------------------------------------------*
      * ANTRAG-1930!        ! NEU FELD DEVISENINLÄNDER    ! RAHNER  *
      *            !        !                             !         *
      *            !        !                             !         *
      *            !        !                             !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBAN0027)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAN0027 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      INHA_NR                        DECIMAL(15, 0) NOT NULL,
CCC004*      INHA_AVERM                     INTEGER NOT NULL,
CCC004*      INHA_VM                        INTEGER NOT NULL,
CCC004*      INHA_LFD_NR                    SMALLINT NOT NULL,
CCC004*      INHA_NAME                      CHAR(26) NOT NULL,
CCC004*      INHA_VNAME                     CHAR(26) NOT NULL,
CCC004*      INHA_STAANG                    SMALLINT NOT NULL,
CCC004*      INHA_PLZ                       CHAR(9) NOT NULL,
CCC004*      INHA_ORT                       CHAR(21) NOT NULL,
CCC004*      INHA_AUSL                      SMALLINT NOT NULL,
CCC004*      INHA_GEB_DAT                   DATE NOT NULL,
CCC004*      INHA_PEP                       CHAR(1) NOT NULL,
CCC004*      INHA_DEVIL                     CHAR(1) NOT NULL,
CCC004*      INHA_BGRP                      SMALLINT NOT NULL,
CCC004*      INHA_BRAN                      SMALLINT NOT NULL,
CCC004*      INHA_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      INHA_LAST_UPDATE               TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.TBAN0027                    *
      ******************************************************************
       01  DCLTBAN0027.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 INHA-NR              PIC S9(15)V USAGE COMP-3.
           10 INHA-AVERM           PIC S9(9) USAGE COMP.
           10 INHA-VM              PIC S9(9) USAGE COMP.
           10 INHA-LFD-NR          PIC S9(4) USAGE COMP.
           10 INHA-NAME            PIC X(26).
           10 INHA-VNAME           PIC X(26).
           10 INHA-STAANG          PIC S9(4) USAGE COMP.
           10 INHA-PLZ             PIC X(9).
           10 INHA-ORT             PIC X(21).
           10 INHA-AUSL            PIC S9(4) USAGE COMP.
           10 INHA-GEB-DAT         PIC X(10).
           10 INHA-PEP             PIC X(1).
           10 INHA-DEVIL           PIC X(1).
           10 INHA-BGRP            PIC S9(4) USAGE COMP.
           10 INHA-BRAN            PIC S9(4) USAGE COMP.
           10 INHA-KENNUNG         PIC X(8).
           10 INHA-LAST-UPDATE     PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************


