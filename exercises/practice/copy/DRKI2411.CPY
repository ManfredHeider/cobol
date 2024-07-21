      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2411.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2411.CPv  $
      *
      *   Rev 2.6   Jun 24 2020 11:10:48   t13ex78
      *FKI01156: PO-DRKI2411-ASTTABELLE  vergrößern
      *
      *   Rev 2.5   Oct 10 2011 15:29:34   t13ex27
      *FKI01156: Wartungsauftrag Abgleich
      *
      *   Rev 2.4   Feb 28 2011 08:50:42   t13ex27
      *AKI01027: Vertragsübersichten im Mandantenwechsel
      *
      *   Rev 2.3   Nov 24 2010 10:31:52   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 26 2010 10:50:40   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   Oct 01 2010 11:58:38   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.0   Jun 28 2010 10:57:56   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2400  FUNKTIONEN UP                      *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00911!02.05.08!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      * AKI01027!31.01.11!2.4 ! Tuning                    !I. Frenzel*
      * FKI01156!10.10.11!2.5 ! PO-DRKI2411-DATEN-INS-IND !I. Frenzel*
      *         !        !    ! vergrößert                !          *
      * FKI01156!24.06.20!2.6 ! PO-DRKI2411-DATEN-INS-IND ! t13ex78  *
      *         !        !    ! vergrößert                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2411.
          05 PI-DRKI2411-PROGRAMM               PIC X(008).

       01 PO-DRKI2411.
          05 PO-DRKI2411-DATEN.
             10 PO-DRKI2411-DATEN-INS OCCURS 200.
                20 PO-DRKI2411-ASTTABELLE           PIC X(30).
                20 PO-DRKI2411-ASTPRODUKTID-V       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTPRODUKTID-B       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTPARTGESELLSCH-V   PIC S9(4) COMP.
                20 PO-DRKI2411-ASTPARTGESELLSCH-B   PIC S9(4) COMP.
                20 PO-DRKI2411-ASTVEZGESCHAEFTSZW   PIC X(2).
                20 PO-DRKI2411-ASTVEZSPARTE-V       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTVEZSPARTE-B       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTOKISPARTE         PIC X(2).
                20 PO-DRKI2411-ASTSTEUERSATZTYP     PIC X(3).
                20 PO-DRKI2411-ASTALLEPRODUKTID     PIC X(1).
             10 PO-DRKI2411-DATEN-INS-CNT           PIC S9(4) COMP.

101011*      10 PO-DRKI2411-DATEN-INS-IND OCCURS 20.
101011*      10 PO-DRKI2411-DATEN-INS-IND OCCURS 30.
             10 PO-DRKI2411-DATEN-INS-IND OCCURS 200.
                20 PO-DRKI2411-ASTTABELLE           PIC X(30).
                20 PO-DRKI2411-ASTTABELLE-IND       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTTABELLE-IND-E     PIC S9(4) COMP.
             10 PO-DRKI2411-DATEN-INS-TAB           PIC S9(4) COMP.

             10 PO-DRKI2411-DATEN-UPD OCCURS 1000.
                20 PO-DRKI2411-ASTTABELLE           PIC X(30).
                20 PO-DRKI2411-ASTATTRIBUT          PIC X(30).
                20 PO-DRKI2411-ASTPRODUKTID-V       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTPRODUKTID-B       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTPARTGESELLSCH-V   PIC S9(4) COMP.
                20 PO-DRKI2411-ASTPARTGESELLSCH-B   PIC S9(4) COMP.
                20 PO-DRKI2411-ASTVEZGESCHAEFTSZW   PIC X(2).
                20 PO-DRKI2411-ASTVEZSPARTE-V       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTVEZSPARTE-B       PIC S9(4) COMP.
                20 PO-DRKI2411-ASTOKISPARTE         PIC X(2).
                20 PO-DRKI2411-ASTSTEUERSATZTYP     PIC X(3).
                20 PO-DRKI2411-ASTALLEPRODUKTID     PIC X(1).

             10 PO-DRKI2411-DATEN-UPD-IND OCCURS 200.
                20 PO-DRKI2411-ASTTABELLE           PIC X(30).
                20 PO-DRKI2411-ASTTABELLE-IND       PIC S9(4) COMP.
             10 PO-DRKI2411-DATEN-UPD-CNT           PIC S9(4) COMP.

          05 PO-DRKI2411-RC                       PIC  X(003).
             88 PO-DRKI2411-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2411-RC-NOK                       VALUE 'NOK'.

