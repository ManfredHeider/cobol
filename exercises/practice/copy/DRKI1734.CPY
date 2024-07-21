      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1734.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1734.CPv  $
      *
      *   Rev 2.1   Jan 10 2005 10:32:18   t13ex27
      *AKI00449: Problematische Bestandskunden in der KoKÜ
      *
      *   Rev 2.0   Oct 15 2004 15:41:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00449!18.10.04!2.1 ! NEU                       ! I.Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *   Daten-Copy für JOIN-OZP ZJKI0023 Funktion DRKI1734         *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1734.
          05 PI-DRKI1734-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1734-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1734-DATEN.
             10 PI-DRKI1734-VRBVERTRAGSID         PIC  X(013).
             10 PI-DRKI1734-KVBKIPERSONENID       PIC  X(013).
      *
       01 PO-DRKI1734.

          05 PO-DRKI1734-RC                       PIC  X(003).
             88 PO-DRKI1734-RC-OK                     VALUE 'OK '.
             88 PO-DRKI1734-RC-NOT-FOUND              VALUE 'NFD'.

          05 PO-DRKI1734-DATEN.
             10 PO-DRKI1734-VRBPERSONENID         PIC X(13).
             10 PO-DRKI1734-VRBKOMMBETRKZ         PIC X(01).
             10 PO-DRKI1734-KVBKUNDEBETREUUNG     PIC X(01).
      *
      ******* ENDE COPY-MEMBER DRKI1734 *****************************
