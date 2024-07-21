      * --------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.02.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2231.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2231.CPv  $
      *
      *   Rev 2.4   Feb 07 2020 12:18:08   t13ac20
      *AKI01764: VDL-Leben: Einführung neuer Pflegetarife Haupttari
      *f/Zusatzversicherung/Bündelprodukt - Erweiterung Einarbeitun
      *
      *
      *
      *   Rev 2.3   Feb 25 2010 09:30:32   t13ex27
      *AKI00894: Erweiterung Einarbeitungsdatei
      *
      *   Rev 2.2   May 29 2008 15:17:02   t13ae49
      *AKI00696: Umstellung Abgleich auf Bind-Variable
      *
      *   Rev 2.1   May 07 2008 11:04:20   t13ae49
      *AKI00696: Umstellung Abgleich auf Bind-Variable
      *
      *   Rev 2.0   Feb 26 2008 08:43:04   i13az13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00696!08.02.08!    ! NEU                       ! JWH      *
      * AKI00894!02.02.10!2.3 ! Umstellung auf variable   ! IFR      *
      *         !        !    ! Satzlänge                 !          *
      * AKI01764!04.02.20!2.4 ! VERGRÖßERUNG DRKI2231-STR ! T13AC20  *
      *         !        !    ! VON 10000 AUF 30000       !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Datenabgleich Host-Oracle - mit Bind SQL
      *
      *
      ****************************************************************

       01  DRKI2231.
020210     10 DRKI2231-STATISTIC.
             15 DRKI2231-LFDNR                   PIC  X(0008).
020210       15 DRKI2231-ENDE-SQL                PIC  9(0004).
020210       15 DRKI2231-ENDE-BIND-V             PIC  9(0004).
020210       15 DRKI2231-ENDE-BIND-W             PIC  9(0004).
020210     10 DRKI2231-STR                       PIC  X(30000).
020210*    10 DRKI2231-SQL-STR                   PIC  X(2500).
020210*    10 DRKI2231-BIND-SQL-STR              PIC  X(2000).
020210*    10 DRKI2231-BIND-VAR-STR              PIC  X(2000).
      *-------------------- ENDE DRKI1866 ----------------------------*

