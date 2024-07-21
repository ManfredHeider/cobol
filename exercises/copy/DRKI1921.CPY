      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1921.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1921.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 17:53:18   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Kundenübe
      *rtragung
      *
      *   Rev 2.1   Sep 14 2006 10:49:30   t13ex48
      *AKI00531: Kundenbetreuungsvereinbarung PGM für KÜ
      *
      *   Rev 2.0   Jun 26 2006 08:29:04   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1921                                     *
      *    Daten-Copy für JOIN-OZP ZJKI0023 Funktion DRKI1921        *
      *    (Kundenuebertragung aus KdBV)                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00531!16.06.06!1.0 ! NEU                       ! Sebbach  *
      *         !23.05.11!    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRKI1921.
          05 PI-DRKI1921-PROGRAMM                PIC  X(008).
          05 PI-DRKI1921-UFKT-CODE               PIC  9(002).
          05 PI-DRKI1921-DATEN.
             10 PI-DRKI1921-KVBVBPERSONENID      PIC  X(013).
      *
       01 PO-DRKI1921.

          05 PO-DRKI1921-RC                      PIC  X(003).
             88 PO-DRKI1921-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1921-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI1921-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1921-DATEN.
             10 PO-DRKI1921-KVBKIPERSONENID      PIC  X(013).
             10 PO-DRKI1921-KNDNUMMER            PIC  9(009).
             10 PO-DRKI1921-KNDSTATUSID          PIC  X(001).
             10 PO-DRKI1921-KNDANZBETREUER       PIC S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER DRKI1921 *****************************
