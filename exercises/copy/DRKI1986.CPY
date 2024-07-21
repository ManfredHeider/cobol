      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1986.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1986.CPv  $
      *
      *   Rev 2.3   Jan 24 2007 16:32:30   t13ex14
      *AKI00638: Bankverbindung für AML BZ ---> VN
      *
      *   Rev 2.0   Nov 06 2006 16:26:48   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1986                                      *
      *    --------------------------------------------------------   *
      *****************************************************************
      *                                                               *
      *  COPY-MEMBER ZUM KORREKTURPROGRAMM PMKI0811                   *
      *   - Eingabedatei AML-Bankverbindungsdaten aus VERTRAGROLLEBEZ *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00638!10.01.07!1.0 !NEU                        !W.Misgeld *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  DRKI1986.
      *
           05  AML-VERTRAGSID                 PIC  X(013).
           05  AML-PERSONENID-BZ              PIC  X(013).
           05  AML-PERSONENROLLEKZ            PIC  X(001).
           05  AML-LFDNR                      PIC S9(009) COMP.
           05  AML-KONTONUMMER                PIC X(015).
           05  AML-BANKBLZID                  PIC S9(009) COMP.
           05  AML-NI-BANKBLZID               PIC  X(001).
           05  AML-KONTOARTID                 PIC S9(004) COMP.
           05  AML-NI-KONTOARTID              PIC  X(001).
           05  AML-INSTITUTBLZ                PIC S9(009) COMP.
           05  AML-NI-INSTITUTBLZ             PIC  X(001).
           05  AML-INTITUTNAME                PIC  X(050).
           05  AML-NI-INTITUTNAME             PIC  X(001).
           05  AML-PARTGESELLSCHID            PIC S9(004) COMP.
           05  AML-NI-PARTGESELLSCHID         PIC  X(001).
      ******* ENDE COPY-MEMBER **************************************
