      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0300.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0300.CPv  $
      *
      *   Rev 2.2   Jan 09 2020 13:54:12   t13ex39
      *AKI01747: EU-DSGVO: Vertragslöschung der Deutschen Bank
      *
      *   Rev 2.1   Jul 05 2004 10:36:00   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 23 2004 10:40:06   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE GELOESCHTEVERTRDAS                                *
      ******************************************************************
CCC005*    EXEC SQL DECLARE GELOESCHTEVERTRDAS TABLE
CCC004*    ( GVDVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      GVDLAUFENDENUMMER              SMALLINT NOT NULL,
CCC004*      GVDGESELLSCHAFT                SMALLINT,
CCC004*      GVDVERTRAGSNUMMER              CHAR(15),
CCC004*      GVDDATUM                       DATE,
CCC004*      GVDNUMMERKZ                    CHAR(1),
CCC004*      GVDVERTRAGSABLAUF              DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GELOESCHTEVERTRDAS          *
      ******************************************************************
       01  GELOESCHTEVERTRDAS-GESAMT.
        05 NI-GELOESCHTEVERTRDAS-TAB.
           10 NI-GELOESCHTEVERTRDAS-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05 NI-GELOESCHTEVERTRDAS REDEFINES NI-GELOESCHTEVERTRDAS-TAB.
           10 NI-GVDVERTRAGSID      PIC S9(4) COMP.
           10 NI-GVDLAUFENDENUMMER  PIC S9(4) COMP.
           10 NI-GVDGESELLSCHAFT    PIC S9(4) COMP.
           10 NI-GVDVERTRAGSNUMMER  PIC S9(4) COMP.
           10 NI-GVDDATUM           PIC S9(4) COMP.
           10 NI-GVDNUMMERKZ        PIC S9(4) COMP.
           10 NI-GVDVERTRAGSABLAUF  PIC S9(4) COMP.
        05 DCL-GELOESCHTEVERTRDAS.
           10 GVDVERTRAGSID        PIC X(13).
           10 GVDLAUFENDENUMMER    PIC S9(4) USAGE COMP.
           10 GVDGESELLSCHAFT      PIC S9(4) USAGE COMP.
           10 GVDVERTRAGSNUMMER    PIC X(15).
           10 GVDDATUM             PIC X(10).
           10 GVDNUMMERKZ          PIC X(1).
           10 GVDVERTRAGSABLAUF    PIC X(10).
      ************************** ENDE-DCLGEN *************************

