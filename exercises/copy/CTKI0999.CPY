      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0999.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0999.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:37:56   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   09 May 1998 12:11:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TMPPERSON)                                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TMPPERSON TABLE
CCC004*    ( TMP_ID                         CHAR(13) NOT NULL,
CCC004*      TMPP_SUCHNAME                  CHAR(10) NOT NULL,
CCC004*      TMPP_SUCHVORNAME               CHAR(10) NOT NULL,
CCC004*      TMPP_GES                       SMALLINT NOT NULL,
CCC004*      TMPP_VNR                       CHAR(15) NOT NULL,
CCC004*      TMPP_ROLLE_VERTRAG             SMALLINT NOT NULL,
CCC004*      TMPP_ROLLE_TARIF               SMALLINT NOT NULL,
CCC004*      TMPP_NNAME                     CHAR(40) NOT NULL,
CCC004*      TMPP_VNAME                     CHAR(20) NOT NULL,
CCC004*      TMPP_GEBDAT                    DATE,
CCC004*      TMPP_ANREDE                    SMALLINT NOT NULL,
CCC004*      TMPP_GESCHL                    CHAR(1) NOT NULL,
CCC004*      TMPP_STR                       CHAR(45) NOT NULL,
CCC004*      TMPP_PLZ                       CHAR(15) NOT NULL,
CCC004*      TMPP_ORT                       CHAR(30) NOT NULL,
CCC004*      TMPP_AUSL_KZ                   CHAR(3) NOT NULL,
CCC004*      TMPP_BERUF                     SMALLINT NOT NULL,
CCC004*      TMPP_KOMM_ART                  SMALLINT NOT NULL,
CCC004*      TMPP_TEL                       CHAR(15) NOT NULL,
CCC004*      TMPP_FAX                       CHAR(15) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TMPPERSON                          *
      ******************************************************************
       01  TMPPERSON-GESAMT.
        05  DCL-TMPPERSON.
           10 TMP-ID               PIC X(13).
           10 TMPP-SUCHNAME        PIC X(10).
           10 TMPP-SUCHVORNAME     PIC X(10).
           10 TMPP-GES             PIC S9(4) USAGE COMP.
           10 TMPP-VNR             PIC X(15).
           10 TMPP-ROLLE-VERTRAG   PIC S9(4) USAGE COMP.
           10 TMPP-ROLLE-TARIF     PIC S9(4) USAGE COMP.
           10 TMPP-NNAME           PIC X(40).
           10 TMPP-VNAME           PIC X(20).
           10 TMPP-GEBDAT          PIC X(10).
           10 NI-TMPP-GEBDAT       PIC S9(4) COMP.
           10 TMPP-ANREDE          PIC S9(4) USAGE COMP.
           10 TMPP-GESCHL          PIC X(1).
           10 TMPP-STR             PIC X(45).
           10 TMPP-PLZ             PIC X(15).
           10 TMPP-ORT             PIC X(30).
           10 TMPP-AUSL-KZ         PIC X(3).
           10 TMPP-BERUF           PIC S9(4) USAGE COMP.
           10 TMPP-KOMM-ART        PIC S9(4) USAGE COMP.
           10 TMPP-TEL             PIC X(15).
           10 TMPP-FAX             PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
